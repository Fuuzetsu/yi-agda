{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      : Yi.Mode.Agda
-- License     : GPL-3
-- Copyright   : © Mateusz Kowalczyk, 2014
-- Maintainer  : fuuzetsu@fuuzetsu.co.uk
-- Stability   : experimental
--
-- Agda mode for Yi

module Yi.Mode.Agda where

import Data.Monoid
import GHC.IO.Exception (ExitCode(..))
import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Data.Attoparsec.Text
import qualified Data.Text as Tx
import qualified Data.Text.IO as TxI
import           Prelude hiding (takeWhile)
import           System.IO
import           System.Process
import           Yi hiding (char)


testFile ∷ FilePath
testFile = "/tmp/DTPiA.agda"

agdaPath ∷ FilePath
agdaPath = "/run/current-system/sw/bin/agda"

data Agda = Agda { _stdIn ∷ Handle
                 , _stdOut ∷ Handle
                 , _stdErr ∷ Handle
                 , _procHandle ∷ ProcessHandle
                 , _threads ∷ [ThreadId]
                 }

type Message = Tx.Text
type Line = Int
type Col = Int
data InfoType = TypeChecking | Error | Goals
              | Other Tx.Text deriving (Show, Eq)

data IdentifierInfo = Keyword | Symbol | PrimitiveType
                    | Module FilePath Int
                    | Function FilePath Int
                    | Datatype FilePath Int
                    | InductiveConstructor FilePath Int
                    | IdentifierOther Tx.Text
                    deriving (Show, Eq)

data Identifier = Identifier Region IdentifierInfo (Maybe FilePath)
                deriving (Show)

data Command = Info InfoType Message Bool
             | HighlightClear
             | HighlightLoadAndDelete FilePath
             | Identifiers [Identifier]
             | Status Tx.Text
             | ErrorGoto Line FilePath Col
             | ParseFailure Tx.Text
             deriving (Show)

identifiers ∷ Parser [Identifier]
identifiers = parens $ identifier `sepBy` char ' '

err ∷ Parser a
err = takeText >>= error . Tx.unpack

err' ∷ a → Parser b
err' = const err

identifier :: Parser Identifier
identifier = parens $ do
  (s, (m, fp)) ← (,) <$> spn <~> idt
  return $ Identifier s m fp
  where
    mfp = Nothing <$ "nil" <|> Just . Tx.unpack <$> str
    withLoc c s = do
      (_,fp,(fp',d)) ← (,,) <$> parens s <~> mfp
                       <~> pair (Tx.unpack <$> str) decimal
      return (c fp' d,fp)
    idt = (,) <$> parens (Keyword <$ "keyword") <~> mfp
          <|> (,) <$> parens (Symbol <$ "symbol") <~> mfp
          <|> (,) <$> parens (PrimitiveType <$ "primitivetype") <~> mfp
          <|> withLoc Module "module"
          <|> withLoc Function "function"
          <|> withLoc Datatype "datatype"
          <|> withLoc InductiveConstructor "inductiveconstructor"
          <|> (,) <$> parens (IdentifierOther <$> takeWhile (/= ')')) <~> mfp

    spn = mkRegion <$> (Point <$> decimal) <~> (Point <$> decimal)

skippingPrompt ∷ Parser ()
skippingPrompt = void . optional $ "Agda2> " *> skipSpace

between ∷ Parser a → Parser b → Parser b
between p = delim p p

delim ∷ Parser a → Parser b → Parser c → Parser c
delim p p' p'' = p *> p'' <* p'

str ∷ Parser Tx.Text
str = between (char '"') $ takeWhile (/= '"')

bool ∷ Parser Bool
bool = False <$ "nil" <|> True <$ "t"

parens ∷ Parser a → Parser a
parens = delim (char '(') (char ')')

quoted ∷ Parser a → Parser a
quoted p = char '\'' *> p

-- | Parser an ELisp pair (naive).
pair ∷ Parser a → Parser b → Parser (a, b)
pair p p' = parens $ (,) <$> (p <* " . ") <*> p'

infixl 4 <~>
-- | Like '(<*>)' but with a single spaces between the parsers, useful
-- for parsing out ‘words’.
(<~>) ∷ Parser (a → b) → Parser a → Parser b
p <~> p' = p <* char ' ' <*> p'

commands ∷ Parser [Command]
commands = command `sepBy` endOfLine

failRest ∷ Parser a
failRest = takeText >>= fail . Tx.unpack

command ∷ Parser Command
command = skippingPrompt *> cmds <|> parseFailure
  where
    cmds = parens (info <|> hlClear <|> hlLoadAndDelete <|> status)
           <|> errorGoto
    parseFailure = ParseFailure <$> takeText
    info = Info <$> ("agda2-info-action " *> infoBfr) <~> str <~> bool
    infoBfr = between (char '"') $
              TypeChecking <$ "*Type-checking*"
              <|> Error <$ "*Error*"
              <|> Goals <$ "*All Goals*"
              <|> Other <$> takeWhile (/= '"')

    hlClear = HighlightClear <$ "agda2-highlight-clear"
    hlLoadAndDelete = HighlightLoadAndDelete . Tx.unpack
                      <$> ("agda2-highlight-load-and-delete-action " *> str)
    status = Status <$> ("agda2-status-action " *> str)
    errorGoto = do
      let ln = snd <$> pair (takeWhile (/= ' ')) decimal
          gt = parens $ "agda2-goto " *> quoted (pair str decimal)
      (l, (f, c)) ← pair ln gt
      return $ ErrorGoto l (Tx.unpack f) c

instance Show Agda where
  show (Agda i o e _ ts) = "Agda " ++ unwords [show i, show o, show e, show ts]

killAgda ∷ Agda → IO ExitCode
killAgda (Agda _ _ _ ph ts) =
  mapM_ killThread ts >> terminateProcess ph >> waitForProcess ph


runAgda ∷ IO Agda
runAgda =
  runInteractiveProcess agdaPath ["--interaction"] Nothing Nothing >>= \case
    (sin', sout, serr, ph) → do
      hSetBuffering sin' NoBuffering
      return $ Agda sin' sout serr ph mempty

parseCommand ∷ String → IO (Either String Command)
parseCommand = return . parseOnly (command <|> failRest) . Tx.pack >=> \case
  Right (HighlightLoadAndDelete fp) →
    TxI.readFile fp >>= return . parseOnly (Identifiers <$> identifiers)
  x → return x

threaded ∷ (Agda → IO ()) → Agda → IO Agda
threaded f a = do
  forkIO (f a) >>= \t → return $ a { _threads = t : _threads a }

parser ∷ Agda → IO ()
parser ag = forever $ hGetLine (_stdOut ag) >>= parseCommand >>= print

test ∷ IO ()
test = do
  ag ← runAgda >>= threaded parser
  send ag $ loadCmd testFile []
  threadDelay 3000000
  void $ killAgda ag

data Activity = Interactive | NonInteractive deriving (Show, Eq)
data Way = Indirect | Direct deriving (Show, Eq)

data IOTCM = IOTCM FilePath Activity Way Cmd
  deriving (Show, Eq)

send ∷ Agda → IOTCM → IO ()
send a = hPutStrLn (_stdIn a) . show

data Cmd where
  Cmd_load ∷ FilePath → [FilePath] → Cmd
  deriving (Show, Eq)

loadCmd ∷ FilePath → [FilePath] → IOTCM
loadCmd fp fps = IOTCM fp NonInteractive Indirect (Cmd_load fp $ "." : fps)
--IOTCM "{{filepath}}" NonInteractive Indirect ( Cmd_goal_type Simplified {{goalIndex}} noRange "" )
