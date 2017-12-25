{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module SlowNews.App where

import           Control.Exception (bracket)
import           Data.Monoid       ((<>))
import           GHC.Generics      (Generic)
import           Katip             (ColorStrategy (ColorIfTerminal),
                                    KatipContextT, LogContexts,
                                    Severity (InfoS), Verbosity (V2),
                                    closeScribes, defaultScribeSettings,
                                    initLogEnv, mkHandleScribe, registerScribe,
                                    runKatipContextT)
import           System.Envy       (DefConfig (defConfig), FromEnv, decodeEnv)
import           System.IO         (stdout)

type Stack a = KatipContextT IO a

data AppEnv = AppEnv
  { port :: Int -- "PORT"
  } deriving (Generic, Show)

instance DefConfig AppEnv where
  defConfig = AppEnv 3000

instance FromEnv AppEnv

runApp :: (AppEnv -> Stack()) -> IO ()
runApp f = do
  handleScribe <- mkHandleScribe ColorIfTerminal stdout InfoS V2
  let mkLogEnv =
        registerScribe "stdout" handleScribe defaultScribeSettings =<<
        initLogEnv "SlowNews" "development"
  appEnvE <- decodeEnv :: IO (Either String AppEnv)
  case appEnvE of
    Left err -> putStrLn $ "Error reading env: " <> err
    Right appEnv ->
      bracket mkLogEnv closeScribes $ \le ->
        runKatipContextT le (mempty :: LogContexts) mempty $ f appEnv

