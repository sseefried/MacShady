-- HSApp: a simple Cocoa app in Haskell
--
-- Tying all components together

import qualified App         as App
import qualified AppDelegate as Delegate
import qualified NSLog       as NSLog

main :: IO ()
main
  = do
    { App.objc_initialise
    ; Delegate.objc_initialise
    ; NSLog.objc_initialise
    ; App.main
    }
