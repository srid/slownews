{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Backend.App where

import Backend.Site (Site)
import Control.Applicative
import Data.Aeson (FromJSON, ToJSON, eitherDecode)
import qualified Data.ByteString.Lazy as B
import Data.Either (either)
import GHC.Generics (Generic)
import System.Directory
import System.Envy (DefConfig (defConfig), FromEnv, decodeEnv)

-- Application environment variables

data Env
  = Env
      { port :: Int -- "PORT"
      }
  deriving (Generic, Eq, Show)

instance DefConfig Env where
  defConfig = Env 3001

instance FromEnv Env

-- Application JSON configuration

data Config
  = Config
      { sites :: [Site]
      }
  deriving (Show, Eq, Generic)

instance FromJSON Config

instance ToJSON Config

loadConfig :: IO (Either String Config)
loadConfig = fmap eitherDecode . B.readFile =<< configFile
  where
    configFile = do
      userConfig <- getXdgDirectory XdgConfig "slownews.json"
      doesPathExist userConfig >>= \case
        True -> pure userConfig
        False -> pure "config/backend/slownews.json"

-- Application data structure

data App
  = App
      { env :: Env,
        config :: Config
      }
  deriving (Show, Eq)

makeApp :: IO App
makeApp =
  either fail pure
    =<< liftA2 App <$> decodeEnv <*> loadConfig
