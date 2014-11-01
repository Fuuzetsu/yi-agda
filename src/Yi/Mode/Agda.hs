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

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Data.Attoparsec.Text
import           Data.Monoid
import qualified Data.Text as Tx
import qualified Data.Text.IO as TxI
import           Prelude hiding (takeWhile)
import           System.Exit (ExitCode(..))
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
data InfoType = TypeChecking | Error | Goals | CurrentGoal
              | Other Tx.Text deriving (Show, Eq)

data IdentifierInfo = Keyword | Symbol | PrimitiveType
                    | Module FilePath Int
                    | Function FilePath Int
                    | Datatype FilePath Int
                    | InductiveConstructor FilePath Int
                    | Bound FilePath Int
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
             | MakeCase [Tx.Text]
             | ParseFailure Tx.Text
             deriving (Show)

identifiers ∷ Parser [Identifier]
identifiers = parens $ identifier `sepBy` char ' '

err ∷ Parser a
err = takeText >>= error . Tx.unpack

err' ∷ a → Parser b
err' = const err

identifier ∷ Parser Identifier
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
          <|> withLoc Bound "bound"
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

-- | Parser for an ELisp pair (naive).
pair ∷ Parser a → Parser b → Parser (a, b)
pair p p' = parens $ (,) <$> (p <* " . ") <*> p'

skipSexpr ∷ Parser ()
skipSexpr = void . parens $ takeWhile (/= ')')

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
           <|> mkCase
           <|> errorGoto
    parseFailure = ParseFailure <$> takeText
    info = Info <$> ("agda2-info-action " *> infoBfr) <~> str <~> bool
    infoBfr = between (char '"') $
              TypeChecking <$ "*Type-checking*"
              <|> Error <$ "*Error*"
              <|> Goals <$ "*All Goals*"
              <|> CurrentGoal <$ "*Current Goal*"
              <|> Other <$> takeWhile (/= '"')

    hlClear = HighlightClear <$ "agda2-highlight-clear"
    hlLoadAndDelete = HighlightLoadAndDelete . Tx.unpack
                      <$> ("agda2-highlight-load-and-delete-action " *> str)
    status = Status <$> ("agda2-status-action " *> str)
    mkCase = let p = parens $ "agda2-make-case-action "
                              *> quoted (parens $ str `sepBy` char ' ')
             in MakeCase . snd <$> pair skipSexpr p
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
  -- send ag $ goalTypeCmd testFile 0
  send ag $ caseCmd testFile 0 109 10 2 110 10 3 "x"
  threadDelay 3000000
  void $ killAgda ag

data Activity = Interactive | NonInteractive deriving (Show, Eq)
data Way = Indirect | Direct deriving (Show, Eq)
data Complexity = Simplified deriving (Show, Eq)

data IOTCM = IOTCM FilePath Activity Way Cmd
  deriving (Show, Eq)

send ∷ Agda → IOTCM → IO ()
send a = sendRaw a . serialise

sendRaw ∷ Agda → String → IO ()
sendRaw a = hPutStrLn (_stdIn a)

data Cmd where
  Cmd_load ∷ FilePath → [FilePath] → Cmd
  Cmd_goal_type ∷ Complexity → Int → Cmd
  Cmd_make_case ∷ FilePath → Int → Int → Int → Int → Int → Int → Int → String
                → Cmd
  deriving (Show, Eq)

serialise ∷ IOTCM → String
serialise fc@(IOTCM fp' act w c) = case c of
  Cmd_load _ _ → show fc
  Cmd_goal_type cmp i → unwords ["IOTCM", show fp', show act, show w
                                , "(Cmd_goal_type", show cmp, show i
                                , "noRange", "\"\")"
                                ]
  Cmd_make_case fp gi sch sr sc ech er ec cnt →
    unwords [ "IOTCM", show fp, show act, show w, "(Cmd_make_case", show gi
            , "(Range [Interval (Pn (Just (mkAbsolute", show fp <> "))"
            , show sch, show sr, show sc <> ")(Pn (Just (mkAbsolute"
            , show fp <> "))", show ech, show er, show ec <> ")])"
            , show cnt <> ")"
            ]

caseCmd ∷ FilePath
        → Int -- ^ Index of the goal
        → Int -- ^ Start char index
        → Int -- ^ Start Row
        → Int -- ^ Start column
        → Int -- ^ End char index
        → Int -- ^ End row
        → Int -- ^ End column
        → String -- ^ Terms you want to split
        → IOTCM
caseCmd fp gi sch sr sc ech er ec cnt = IOTCM fp NonInteractive Indirect (Cmd_make_case fp gi sch sr sc ech er ec cnt)

loadCmd ∷ FilePath → [FilePath] → IOTCM
loadCmd fp fps = IOTCM fp NonInteractive Indirect (Cmd_load fp $ "." : fps)

goalTypeCmd ∷ FilePath
            → Int -- ^ Goal index
            → IOTCM
goalTypeCmd fp i = IOTCM fp NonInteractive Indirect (Cmd_goal_type Simplified i)
