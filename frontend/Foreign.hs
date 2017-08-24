module Foreign where

import           Miso.String                   hiding (reverse)
    
foreign import javascript unsafe "console.log ($1);"
  consoleLog :: MisoString -> IO ()
