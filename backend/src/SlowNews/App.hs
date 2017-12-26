{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module SlowNews.App where

import           Control.Exception    (bracket)
import           Data.Monoid          ((<>))
import           GHC.Generics         (Generic)
import           Katip                (KatipContextT, LogContexts,
                                       Severity (InfoS), Verbosity (V2),
                                       closeScribes, runKatipContextT)
import           Katip.Scribes.Handle (ioLogEnv)
import           System.Envy          (DefConfig (defConfig), FromEnv,
                                       decodeEnv)

type Stack a = KatipContextT IO a

data AppEnv = AppEnv
  { port :: Int -- "PORT"
  } deriving (Generic, Show)

instance DefConfig AppEnv where
  defConfig = AppEnv 3000

instance FromEnv AppEnv

runApp :: (AppEnv -> Stack()) -> IO ()
runApp f = do
  appEnvE <- decodeEnv :: IO (Either String AppEnv)
  case appEnvE of
    Left err -> putStrLn $ "Error reading env: " <> err
    Right appEnv -> do
      let mkLogEnv = ioLogEnv InfoS V2
      bracket mkLogEnv closeScribes $ \le ->
        runKatipContextT le (mempty :: LogContexts) mempty $ f appEnv
