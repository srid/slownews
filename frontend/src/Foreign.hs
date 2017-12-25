module Foreign where

import           Miso.String (MisoString)

foreign import javascript unsafe "console.log ($1);"
  consoleLog :: MisoString -> IO ()
