{-# LANGUAGE GADTs #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE LambdaCase #-}
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

import Control.Concurrent
import Control.Monad
import System.IO
import System.Directory
import System.Process
import System.Environment
import Yi

testFile ∷ FilePath
testFile = "/tmp/DTPiA.agda"

agdaPath ∷ FilePath
agdaPath = "/run/current-system/sw/bin/agda"

data Agda = Agda { _stdIn ∷ Handle
                 , _stdOut ∷ Handle
                 , _stdErr ∷ Handle
                 , _procHandle ∷ ProcessHandle
                 }

runAgda ∷ IO Agda
runAgda =
  runInteractiveProcess agdaPath ["--interaction"] Nothing Nothing >>= \case
    (sin, sout, serr, ph) → return $ Agda sin sout serr ph

test ∷ IO ()
test = do
  a@(Agda sin sout serr ph) ← runAgda
  tr ← forkIO . forever $ hGetLine sout >>= print
  loadTest a
  threadDelay 100000
  killThread tr
  terminateProcess ph

data Activity = Interactive | NonInteractive deriving (Show, Eq)
data Way = Indirect | Direct deriving (Show, Eq)

data IOTCM = IOTCM FilePath Activity Way Cmd
  deriving (Show, Eq)

data Cmd where
  Cmd_load ∷ FilePath → [FilePath] → Cmd
  deriving (Show, Eq)

loadTest :: Agda → IO ()
loadTest (Agda sin _ _ _) = hPutStrLn sin (show $ loadCmd testFile [])

loadCmd ∷ FilePath → [FilePath] → IOTCM
loadCmd fp fps = IOTCM fp NonInteractive Indirect (Cmd_load fp $ "." : fps)
