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
import           Data.Bits
import           Data.Default
import           Data.Either
import           Data.IORef
import           Data.Monoid
import qualified Data.Text as Tx
import qualified Data.Text.Encoding as TxE
import qualified Data.Text.IO as TxI
import           Data.Typeable
import           Prelude hiding (takeWhile, drop)
import           System.Directory
import           System.Exit (ExitCode(..))
import           System.IO
import           System.Process
import           Yi hiding (char)
import           Yi.Config.Simple.Types
import           Yi.Keymap.Emacs.KillRing
import           Yi.Lexer.Alex
import           Yi.Modes
import qualified Yi.Rope as R
import           Yi.String
import           Yi.Types (YiVariable, YiConfigVariable)

data AgdaStyle = AgdaStyle
  { _agdaKeyword ∷ StyleName
  , _agdaSymbol ∷ StyleName
  , _agdaPrimitiveType ∷ StyleName
  , _agdaModule ∷ StyleName
  , _agdaFunction ∷ StyleName
  , _agdaDatatype ∷ StyleName
  , _agdaInductiveConstructor ∷ StyleName
  , _agdaBound ∷ StyleName
  , _agdaUnsolvedMeta ∷ StyleName
  , _agdaPostulate ∷ StyleName
  , _agdaTerminationProblem ∷ StyleName
  , _agdaError ∷ StyleName
  , _agdaString ∷ StyleName
  } deriving (Typeable)


-- | Convenience function
rgb :: Word32 -> Color
rgb x = RGB (fi (x `shiftR` 16))
            (fi (x `shiftR` 8))
            (fi x)
  where
    fi = fromIntegral

fgColour ∷ Word32 → b → Style
fgColour x = const $ withFg (rgb x)

bgColour ∷ Word32 → b → Style
bgColour x = const $ withBg (rgb x)

-- | Agda defaults are used except in places where there's already a
-- matching attribute in 'UIStyle'.
instance Default AgdaStyle where
  def = AgdaStyle { _agdaKeyword = keywordStyle
                  , _agdaSymbol = fgColour 0x404040 -- gray75
                  , _agdaPrimitiveType = builtinStyle
                  , _agdaModule = fgColour 0xa020f0 -- purple
                  , _agdaFunction = fgColour 0x0000cd -- medium blue
                  , _agdaDatatype = typeStyle
                  , _agdaInductiveConstructor = fgColour 0x008b00 -- green4
                  , _agdaBound = defaultStyle
                  , _agdaUnsolvedMeta = -- yellow bg, black fg
                      bgColour 0xffff00 <> fgColour 0x000000
                  , _agdaPostulate = fgColour 0x0000cd -- medium blue
                  , _agdaTerminationProblem = -- light salmon bg, black fg
                    bgColour 0xffa07a <> fgColour 0x000000
                  , _agdaError = fgColour 0xff0000 -- red
                  , _agdaString = fgColour 0xb22222 -- firebrick
                  }

instance YiConfigVariable AgdaStyle

data GoalInfo = GoalInfo { _goalIndex ∷ !Int
                         , _goalType ∷ Tx.Text
                         } deriving (Show, Eq, Typeable)

getText ∷ Get Tx.Text
getText = TxE.decodeUtf8 <$> Data.Binary.get

putText ∷ Tx.Text → Put
putText = Data.Binary.put . TxE.encodeUtf8

instance Binary GoalInfo where
  put (GoalInfo i gt) = Data.Binary.put i *> putText gt
  get = GoalInfo <$> Data.Binary.get <*> getText

data AgdaState = AgdaState
  { _agdaGoals ∷ [GoalInfo]
  } deriving (Show, Eq, Typeable)

instance Default AgdaState where
  def = AgdaState { _agdaGoals = mempty }

instance Binary AgdaState where
  put (AgdaState gs) = Data.Binary.put gs
  get = AgdaState <$> Data.Binary.get

instance YiVariable AgdaState

agdaStyle ∷ Field AgdaStyle
agdaStyle = customVariable

styleIdentifier ∷ AgdaStyle → IdentifierInfo → StyleName
styleIdentifier as Keyword = _agdaKeyword as
styleIdentifier as Symbol = _agdaSymbol as
styleIdentifier as PrimitiveType = _agdaPrimitiveType as
styleIdentifier as (Module _ _) = _agdaModule as
styleIdentifier as (Function _ _) = _agdaFunction as
styleIdentifier as (Datatype _ _) = _agdaDatatype as
styleIdentifier as (InductiveConstructor _) = _agdaInductiveConstructor as
styleIdentifier as (Bound _ _) = _agdaBound as
styleIdentifier as (Postulate _ _) = _agdaPostulate as
styleIdentifier as (TerminationProblem _ _) = _agdaTerminationProblem as
styleIdentifier as UnsolvedMeta = _agdaUnsolvedMeta as
styleIdentifier as (ErrorI _ _ _ _) = _agdaError as
styleIdentifier as StringI = _agdaString as
styleIdentifier _ (IdentifierOther _) = defaultStyle

sl ∷ AgdaStyle → StyleLexerASI () IdentifierInfo
sl st = StyleLexer { _tokenToStyle = styleIdentifier st
                   , _styleLexer = commonLexer (const Nothing) ()
                   }

agdaMode ∷ TokenBasedMode IdentifierInfo
agdaMode = mkAgdaMode $ sl def

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
    fwriteBufferE (bkey b) -- save before loading in the file
    sendAgda' (runCommands b) . loadCmd fp $ _agdaIncludeDirs d

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
runCommands b (Status m:cs) | Tx.null m = runCommands b cs
                            | otherwise = printMsg m >> runCommands b cs
runCommands _ (ErrorGoto _ fp pnt:_) =
  -- TODO: It seems we should delay this until we processed all other
  -- commands to: don't want to miss out on highlighting because a
  -- goto came first.
  openingNewFile fp $ moveTo (Point pnt)
-- Insert replacements at point
runCommands b (MakeCase ls:cs) = do
  withGivenBuffer (bkey b) . savingPointB $ do
    pointB >>= solPointB >>= moveTo
    killRestOfLine
    insertN (R.fromText $ Tx.unlines ls)
  runCommands b cs
runCommands b (HighlightClear:cs) = do
  withGivenBuffer (bkey b) $ delOverlaysOfOwnerB "agda"
  runCommands b cs
runCommands b a@(Identifiers _:_) = do
  let allIds = concat [ i | Identifiers i ← a ]
      rest = filter (\case { Identifiers _ → False; _ → True }) a
  overlays ← withEditor $ mapM makeOverlay allIds
  withGivenBuffer (bkey b) $ mapM_ addOverlayB overlays
  runCommands b rest
runCommands b (ParseFailure t:cs) = printMsg t >> runCommands b cs
runCommands b (HighlightLoadAndDelete fp:cs) =
  printMsg ("Somehow didn't read in " <> Tx.pack fp) >> runCommands b cs
runCommands b (GoalAction _:cs) = runCommands b cs

-- | Turns identifiers Agda tells us about to colour overlays.
--
-- Uses current 'agdaStyle'.
makeOverlay ∷ Identifier → EditorM Overlay
makeOverlay (Identifier r i _) =
  agdaStyle `views` \st → mkOverlay "agda" r (styleIdentifier st i) ""

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
    cb ← withCurrentBuffer $ gets bkey
    b ← startSubprocess binaryPath
          (extraFlags <> ["--interaction"]) handleExit
    -- Switch back to the buffer we were in, startSubprocess really
    -- shouldn't switch for us.
    withEditor $ switchToBufferE cb
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

        -- Try set number of times before announcing Agda is not ready
        -- with a small wait between them, currently 10ms.
        ready ∷ Int → YiM (Either Tx.Text Int)
        ready n = withGivenBuffer b endIsPrompt >>= \case
          Nothing | n > 0 → liftBase (threadDelay 20000) >> ready (n - 1)
                  | otherwise →
                      return $ Left "Agda is not ready, not sending command"
          Just i → return $ Right i

    liftBase . putStrLn $ "Sending: " <> s
    t ← ready 10

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
               liftBase $ putStrLn (R.toString $ R.unlines s')
               cs ← fmap rights . liftBase $ mapM (parseCommand . R.toText) s'
               void $ f cs
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
                    | InductiveConstructor (Maybe (FilePath, Int))
                    | Bound FilePath Int
                    | IdentifierOther Tx.Text
                    | Postulate FilePath Int
                    | TerminationProblem FilePath Int
                    | UnsolvedMeta
                    | ErrorI FilePath Line Col Tx.Text
                    | StringI
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
             | GoalAction [Int]
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
    errP = do
      s ← parens "error" *> char ' ' *> str
      let p ∷ Parser (FilePath, Int, Int, Tx.Text)
          p = do
            fp ← takeWhile (/= ':') <* char ':'
            (ln, cl) ← (,) <$> (decimal <* char ',') <*> decimal
            -- Stuff is escaped (notably newline) because ELisp.
            i ← char '-' *> (decimal ∷ Parser Int) *> "\\n" *> takeText
            return (Tx.unpack fp, ln, cl, i)
      case parseOnly p s of
       Left s' → fail s'
       Right (fp, ln, cl, i) → return $ ErrorI fp ln cl i

    inductive = do
      (_,fp,p) ← (,,) <$> parens "inductiveconstructor" <~> mfp
                 <*> optional (char ' ' *> pair (Tx.unpack <$> str) decimal)
      return $ (InductiveConstructor p, fp)
    idt = (,) <$> parens (Keyword <$ "keyword") <~> mfp
          <|> (,) <$> parens (Symbol <$ "symbol") <~> mfp
          <|> (,) <$> parens (PrimitiveType <$ "primitivetype") <~> mfp
          <|> (,) <$> parens (UnsolvedMeta <$ "unsolvedmeta") <~> mfp
          <|> (,) <$> parens (StringI <$ "string") <~> mfp
          <|> withLoc Module "module"
          <|> withLoc Function "function"
          <|> withLoc Datatype "datatype"
          <|> inductive
          <|> withLoc Bound "bound"
          <|> withLoc Postulate "postulate"
          <|> withLoc TerminationProblem "terminationproblem function"
          <|> (,) <$> errP <*> return Nothing
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

-- | Parses out stuff between double quotes. Accounts for quotes
-- escaped with a backslash.
str ∷ Parser Tx.Text
str = do
  let p = "\\\"" <|> Tx.pack . return <$> notChar '"'
  char '"' *> (mconcat <$> many p) <* char '"'

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
           <|> mkGoalAct
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
    mkGoalAct = let p = parens $ "agda2-goals-action "
                              *> quoted (parens $ decimal `sepBy` char ' ')
             in GoalAction . snd <$> pair skipSexpr p
    mkCase = let p = parens $ "agda2-make-case-action "
                              *> quoted (parens $ str `sepBy` char ' ')
             in MakeCase . snd <$> pair skipSexpr p
    errorGoto = do
      let ln = snd <$> pair (takeWhile (/= ' ')) decimal
          gt = parens $ "agda2-goto " *> quoted (pair str decimal)
      (l, (f, pnt)) ← pair ln gt
      return $ ErrorGoto l (Tx.unpack f) pnt

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
    fc ← TxI.readFile fp
    let r = parseOnly (Identifiers <$> identifiers) fc
    case r of
      Left _ → putStrLn $ Tx.unpack fc
      _ → return ()
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
  threadDelay 500000
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
