{-# LANGUAGE OverloadedStrings #-}
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
import Yi.MiniBuffer (promptingForBuffer)
import Yi.Config.Simple (globalBindKeys)
import Yi.Config.Simple.Types
import Yi.Config.Default (defaultConfig)
import Yi.Config.Default.HaskellMode (configureHaskellMode)
import Yi.Config.Default.MiscModes (configureMiscModes)
import Yi.Config.Default.Vty (configureVty)
import Yi.Config.Default.Cua (configureCua)

{-
  TODOS:
  * Tab handling.
  * Splitting horizontally, vertically.
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

switchBuffer :: YiM ()
switchBuffer = promptingForBuffer "buffer name:"
    (withEditor . switchToBufferE)
    (\o a -> (a \\ o) ++ o)

-- | Kill the rest of the line
killRestOfLine :: BufferM ()
killRestOfLine =
    do eol <- atEol
       if eol then deleteN 1 else deleteToEol

