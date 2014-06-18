-- HSApp: a simple Cocoa app in Haskell
--
-- Tying all components together

import qualified App         as App
import qualified AppDelegate as AppDelegate
import qualified NSLog       as NSLog

-- FIXME: These imports might not be necessary later
import           MSState
import           Shady.CompileEffect (testEffect)

main :: IO ()
main = do
  initMSEffect testEffect -- FIXME: Remove later
  App.objc_initialise
  AppDelegate.objc_initialise
  NSLog.objc_initialise
  App.main

