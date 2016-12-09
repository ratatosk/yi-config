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
import Yi.Config.Simple (globalBindKeys)
import Yi.Config.Simple.Types
import Yi.Config.Default (defaultConfig)
import Yi.Config.Default.HaskellMode (configureHaskellMode)
import Yi.Config.Default.MiscModes (configureMiscModes)
import Yi.Config.Default.Vty (configureVty)
import Yi.Config.Default.Cua (configureCua)

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

-- | Kill the rest of the line
killRestOfLine :: BufferM ()
killRestOfLine =
    do eol <- atEol
       if eol then deleteN 1 else deleteToEol

