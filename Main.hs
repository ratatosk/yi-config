{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
import Control.Monad.State.Lazy
import Data.List
import Lens.Micro.Platform
import System.Environment

import Yi.Buffer
import Yi.Core
import Yi.Config
import Yi.Editor
import Yi.File
import Yi.Keymap
import Yi.Keymap.Keys
import Yi.MiniBuffer (promptingForBuffer, spawnMinibufferE)
import Yi.Config.Simple (globalBindKeys)
import Yi.Config.Simple.Types
import Yi.Config.Default (defaultConfig)
import Yi.Config.Default.HaskellMode (configureHaskellMode)
import Yi.Config.Default.MiscModes (configureMiscModes)
import Yi.Config.Default.Vty (configureVty)
import Yi.Config.Default.Cua (configureCua)

{-
  TODOS:
  * Keep indent on Enter.
  * Splitting horizontally, vertically.
  * Make config a record rather than monadic thing.
  * Remove dependency on Cua.
  * Buggy keys:
    - Ctrl+[ in uxterm/urxvt
    - Shift+Up/Down in urxvt
  * Breaks unicode in uxterm
  * Closing.
  * Place of last edit?
  * Prolog mode.
  * Smarter buffer selector (Ctrl-E):
    - case insensitive.
    - fuzzy (common subsequence).
    - don't create new buffer, pick top one on Enter.
    - handle arrows.
  * Separate new buffer command.
  * Highlight/kill-on-save trailing whitespace.
  * Save set of opened files, reopen them.
  * Smarter Tab handling - cycle through previous indents and last + Nspaces.
  * Mouse scroll
-}

main :: IO ()
main = do
    files <- getArgs
    let openFileActions = intersperse (EditorA newTabE) (map (YiA . openNewFile) files)
    cfg <- execStateT
        (runConfigM (myConfig >> (startActionsA .= openFileActions)))
        defaultConfig
    startEditor cfg Nothing

myConfig :: ConfigM ()
myConfig = do
    configureVty
    configureCua
    configureHaskellMode
    configureMiscModes
    globalBindKeys $ ctrlCh 'k' ?>>! killRestOfLine
    globalBindKeys $ ctrlCh 'e' ?>>! switchBuffer
    globalBindKeys $ ctrlCh 'w' ?>>! killCurrentBuffer
    globalBindKeys $ spec KTab ?>>! autoIndentB IncreaseCycle

-- | Kill current buffer asking to save if needed.
killCurrentBuffer :: YiM ()
killCurrentBuffer = do
    buf <- gets currentBuffer
    askSave <- needsSave buf
    withEditor $ if askSave
        then void $ spawnMinibufferE question (minibufKeymap buf)
        else deleteBuffer buf
  where
    minibufKeymap buf =
        const $ choice [ char 'n' ?>>! deleteBuffer buf >> closeBufferAndWindowE
                       , char 'y' ?>>! saveAndClose buf
                       , char 'c' ?>>! closeBufferAndWindowE
                       ]
    needsSave buf = do
        fBuf <- withEditor $ gets $ findBufferWith buf
        deservesSave fBuf
    saveAndClose buf = do
        saved <- fwriteBufferE buf
        withEditor $ do
            when saved $ deleteBuffer buf
            closeBufferAndWindowE
    question = "Buffer modified, save (Yes/No/Cancel)?"

-- | Switch to other opened buffer (asks in minibuffer)
switchBuffer :: YiM ()
switchBuffer = promptingForBuffer "buffer name:"
    (withEditor . switchToBufferE)
    (\o a -> (a \\ o) ++ o)

-- | Kill the rest of the line
killRestOfLine :: BufferM ()
killRestOfLine =
    do eol <- atEol
       if eol then deleteN 1 else deleteToEol

