{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
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
import           Control.Lens hiding (act)
import           Control.Monad.Base
import           Control.Monad.State
import           Data.Attoparsec.Text
import           Data.Binary
import           Data.Default
import           Data.Either
import           Data.IORef
import           Data.Monoid
import qualified Data.Text as Tx
import qualified Data.Text.IO as TxI
import           Data.Typeable
import           Prelude hiding (takeWhile)
import           System.Directory
import           System.Exit (ExitCode(..))
import           System.IO
import           System.Process
import           Yi hiding (char)
import           Yi.Keymap.Emacs.KillRing
import           Yi.Lexer.Alex
import           Yi.Modes
import qualified Yi.Rope as R
import           Yi.String
import           Yi.Types (YiVariable)

idToStyle ∷ IdentifierInfo → StyleName
idToStyle Keyword = keywordStyle
idToStyle PrimitiveType = builtinStyle
idToStyle (Datatype _ _) = typeStyle
idToStyle _ = defaultStyle

sl ∷ StyleLexerASI () IdentifierInfo
sl = StyleLexer { _tokenToStyle = idToStyle
                , _styleLexer = commonLexer (const Nothing) ()
                }

agdaMode ∷ TokenBasedMode IdentifierInfo
agdaMode = mkAgdaMode sl

-- | Re-make the Agda mode: this allows us to cheat and when we want
-- to slide in a new lexer, we remake the whole thing.
mkAgdaMode ∷ Show (l s) ⇒ StyleLexer l s t i -> TokenBasedMode t
mkAgdaMode x = styleMode x
  & modeNameA .~ "agda"
  & modeAppliesA .~ anyExtension [ "lagda", "agda" ]
  & modeToggleCommentSelectionA .~ Just (toggleCommentB "--")

loadCurrentBuffer ∷ YiM ()
loadCurrentBuffer = withCurrentBuffer (gets file) >>= \case
  Nothing → printMsg "Current buffer is not associated with a file."
  Just fp → do
    b ← withCurrentBuffer $ gets id
    d ← getAgda >> getEditorDyn
    sendAgda' (runCommands b) . loadCmd fp . _agdaIncludeDirs $ d

splitCase ∷ YiM ()
splitCase = withCurrentBuffer (gets file) >>= \case
  Nothing → printMsg "Current buffer is not associated with a file."
  Just fp → do
    (Point p, c, l, cr) ←
      withCurrentBuffer $ (,,,) <$> pointB <*> curCol <*> curLn <*> readB
    b ← withCurrentBuffer $ gets id
    sendAgda' (runCommands b) $ caseCmd fp 0 p l c (p + 1) l (c + 1) [cr]

-- | Given commands, actually execute them in the editor
runCommands ∷ FBuffer → [Command] → YiM ()
runCommands _ [] = return ()
runCommands b (Info _ m _:cs) = printMsg m >> runCommands b cs
runCommands b (Status m:cs) = printMsg m >> runCommands b cs
runCommands _ (ErrorGoto ln fp cl:_) =
  -- TODO: It seems we should delay this until we processed all other
  -- commands to: don't want to miss out on highlighting because a
  -- goto came first.
  openingNewFile fp $ moveToLineColB ln cl
-- Insert replacements at point
runCommands b (MakeCase ls:cs) = do
  withGivenBuffer (bkey b) . savingPointB $ do
    pointB >>= solPointB >>= moveTo
    killRestOfLine
    insertN (R.fromText $ Tx.unlines ls)
  runCommands b cs
runCommands b (HighlightClear:cs) = do
  withGivenBuffer (bkey b) $ delOverlayLayerB UserLayer
  runCommands b cs
runCommands b a@(Identifiers _:_) = do
  let allIds = concat [ i | Identifiers i ← a ]
      rest = filter (\case { Identifiers _ → False; _ → True }) a
  _ ← withGivenBuffer (bkey b) $ mapM (addOverlayB . makeOverlay) allIds
      -- let oldKm = withMode0 modeKeymap b
      -- setMode $ mkAgdaMode (sl & styleLexer .~ commonLexer fetch ())
      --         & modeKeymapA .~ oldKm

  runCommands b rest
runCommands b (ParseFailure t:cs) = printMsg t >> runCommands b cs
runCommands b (HighlightLoadAndDelete fp:cs) =
  printMsg ("Somehow didn't read in " <> Tx.pack fp) >> runCommands b cs


-- | Turns identifiers Agda tells us about to colour overlays.
makeOverlay ∷ Identifier → Overlay
makeOverlay (Identifier r i _) = mkOverlay UserLayer r (idToStyle i)

-- | Buffer used for process communication between Yi and Agda. To me
-- it seems like the interface isn't flexible enough.
newtype AgdaBuffer = AgdaBuffer { _agdaBuffer ∷ Maybe BufferRef }
                   deriving (Show, Eq, Typeable)

instance Default AgdaBuffer where
  def = AgdaBuffer Nothing

-- | Path to the Agda binary.
newtype AgdaPath = AgdaPath { _agdaPath ∷ FilePath }
                 deriving (Show, Eq, Typeable)

instance Default AgdaPath where
  def = AgdaPath "agda"

-- | Directories to pass to Agda commands, pointing at libraries needed.
newtype AgdaIncludeDirs = AgdaIncludeDirs { _agdaIncludeDirs ∷ [FilePath] }
                        deriving (Show, Eq, Typeable)

-- | By default, we point to the the "." directory.
instance Default AgdaIncludeDirs where
  def = AgdaIncludeDirs ["."]

-- | Extra flags to pass to Agda when it starts. "--interaction" is
-- always used regardless and passed as the last element.
newtype AgdaExtraFlags = AgdaExtraFlags { _agdaExtraFlags ∷ [String] }
                       deriving (Show, Eq, Typeable)

-- | By default no extra flags are passed in
instance Default AgdaExtraFlags where
  def = AgdaExtraFlags []

deriving instance Binary AgdaBuffer
deriving instance Binary AgdaPath
deriving instance Binary AgdaIncludeDirs
deriving instance Binary AgdaExtraFlags
instance YiVariable AgdaBuffer
instance YiVariable AgdaPath
instance YiVariable AgdaIncludeDirs
instance YiVariable AgdaExtraFlags

startAgda ∷ YiM BufferRef
startAgda = getEditorDyn >>= \case
  AgdaBuffer (Just b) → printMsg "Agda already started" >> return b
  AgdaBuffer Nothing → do
    AgdaPath binaryPath ← getEditorDyn
    AgdaExtraFlags extraFlags ← getEditorDyn
    b ← startSubprocess binaryPath
          (extraFlags <> ["--interaction"]) handleExit
    putEditorDyn (AgdaBuffer $ Just b) >> return b

 where
   handleExit (Right ExitSuccess) = printMsg "Agda closed gracefully"
   handleExit (Right (ExitFailure x)) = printMsg $ "Agda quit with: " <> showT x
   handleExit (Left e) = printMsg $ "Exception in the Agda process: " <> showT e

getAgda ∷ YiM BufferRef
getAgda = getEditorDyn >>= \case
  AgdaBuffer (Just b) → return b
  AgdaBuffer Nothing → startAgda

sendAgda ∷ IOTCM → YiM ()
sendAgda = sendAgda' (const $ return ())

sendAgda' ∷ ([Command] → YiM a) → IOTCM → YiM ()
sendAgda' f = sendAgdaRaw f . serialise

-- | This monster sends a command to Agda, waits until a new prompt is
-- spotted, parses everything in between and runs user-supplied
-- function on the results.
sendAgdaRaw ∷ ([Command] → YiM a) → String → YiM ()
sendAgdaRaw f s = getEditorDyn >>= \case
  AgdaBuffer Nothing → printMsg "Agda is not running."
  AgdaBuffer (Just b) → do
    let endIsPrompt ∷ BufferM (Maybe Int)
        endIsPrompt = do
          Point i ← sizeB
          end ← betweenB (Point $ i - Tx.length promptTxt) (Point i)
          return $ if R.toText end == promptTxt then Just i else Nothing

    liftBase . putStrLn $ "Sending: " <> s
    t ← withGivenBuffer b $ endIsPrompt >>= return . \case
      Nothing → Left "Agda is not ready, not sending command"
      Just i → Right i

    case t of
     Left m → printMsg m
     Right bfs → do
       ior ← liftBase $ newIORef True
       sendToProcess b (s <> "\n")
       let terminate = do
             c ← readIORef ior
             if c then threadDelay 10000 >> return True else return False

           wb ∷ MonadEditor m ⇒ BufferM a → m a
           wb = withGivenBuffer b

           loop ∷ YiM ()
           loop = wb endIsPrompt >>= \case
             Nothing → return ()
             Just nbfs → when (nbfs /= bfs) $ do
               liftBase $ writeIORef ior False
               s' ← R.lines <$> wb (betweenB (Point bfs) (Point nbfs))
               cs ← fmap rights . liftBase $ mapM (parseCommand . R.toText) s'
               void $ f cs
               ems ← R.toString <$> withGivenBuffer b elemsB
               liftBase $ putStrLn ems

       void $ forkAction terminate MustRefresh loop

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

    -- Emacs counts columns with different indexing so we need to
    -- compensate here
    spn = mkRegion <$> (Point . pred <$> decimal) <~> (Point . pred <$> decimal)

skippingPrompt ∷ Parser ()
skippingPrompt = void $ optional prompt

promptTxt ∷ Tx.Text
promptTxt = "Agda2> "

prompt ∷ Parser Tx.Text
prompt = string promptTxt <* skipSpace

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

parseCommand ∷ Tx.Text → IO (Either String Command)
parseCommand = return . parseOnly (command <|> failRest) >=> \case
  Right (HighlightLoadAndDelete fp) → do
    r ← TxI.readFile fp >>= return . parseOnly (Identifiers <$> identifiers)
    removeFile fp >> return r
  x → return x

threaded ∷ (Agda → IO ()) → Agda → IO Agda
threaded f a = do
  forkIO (f a) >>= \t → return $ a { _threads = t : _threads a }

parser ∷ Agda → IO ()
parser ag = forever $ do
  l ← hGetContents (_stdOut ag)
  mapM_ (parseCommand . Tx.pack >=> print) (lines l)

test ∷ IO ()
test = do
  ag ← runAgda >>= threaded parser
  send ag $ loadCmd testFile []
  send ag $ goalTypeCmd testFile 0
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
caseCmd fp gi sch sr sc ech er ec cnt =
  IOTCM fp NonInteractive Indirect (Cmd_make_case fp gi sch sr sc ech er ec cnt)

loadCmd ∷ FilePath → [FilePath] → IOTCM
loadCmd fp fps = IOTCM fp NonInteractive Indirect (Cmd_load fp fps)

goalTypeCmd ∷ FilePath
            → Int -- ^ Goal index
            → IOTCM
goalTypeCmd fp i = IOTCM fp NonInteractive Indirect (Cmd_goal_type Simplified i)
