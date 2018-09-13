{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Backend.App where

import Data.Aeson (FromJSON, ToJSON, eitherDecode)
import qualified Data.ByteString.Lazy as B
import Data.Either (either)
import GHC.Generics (Generic)
import System.Directory
import System.Envy (DefConfig (defConfig), FromEnv, decodeEnv)

import Backend.Site (Site)

-- Application environment variables

data Env = Env
  { port :: Int -- "PORT"
  } deriving (Generic, Eq, Show)

instance DefConfig Env where
  defConfig = Env 3001

instance FromEnv Env

-- Application JSON configuration

data Config = Config
  { sites :: [Site]
  } deriving (Show, Eq, Generic)

instance FromJSON Config
instance ToJSON Config

loadConfig :: IO (Either String Config)
loadConfig = do
  userConfig <- getXdgDirectory XdgConfig "slownews.json"
  f <- doesPathExist userConfig >>= \case
    True -> pure userConfig
    False -> pure "config/backend/slownews.json"
  content <- B.readFile f
  pure $ eitherDecode content

-- Application data structure

data App = App
  { env    :: Env
  , config :: Config
  } deriving (Show, Eq)

makeApp :: IO App
makeApp = do
  appEnvE <- decodeEnv
  configE <- loadConfig
  either fail pure $ App <$> appEnvE <*> configE
