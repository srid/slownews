{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Common.Route where

import Prelude hiding ((.))

import Control.Category
import Control.Monad.Except
import Data.Functor.Sum
import Data.Some (Some)
import qualified Data.Some as Some
import Data.Text (Text)

import Obelisk.Route
import Obelisk.Route.TH

backendRouteEncoder
  :: ( check ~ parse
     , MonadError Text parse
     )
  => Encoder check parse (R (Sum BackendRoute (ObeliskRoute Route))) PageName
backendRouteEncoder = Encoder $ do
  let myComponentEncoder = (backendRouteComponentEncoder `shadowEncoder` obeliskRouteComponentEncoder routeComponentEncoder) . someSumEncoder
  myObeliskRestValidEncoder <- checkObeliskRouteRestEncoder routeRestEncoder
  checkEncoder $ pathComponentEncoder myComponentEncoder $ \case
    InL backendRoute -> case backendRoute of
      BackendRoute_GetData -> endValidEncoder mempty
    InR obeliskRoute -> runValidEncoderFunc myObeliskRestValidEncoder obeliskRoute

--TODO: Should we rename `Route` to `AppRoute`?
data BackendRoute :: * -> * where
  BackendRoute_GetData :: BackendRoute ()

data Route :: * -> * where
  Route_Home :: Route ()

backendRouteComponentEncoder :: (MonadError Text check, MonadError Text parse) => Encoder check parse (Some BackendRoute) (Maybe Text)
backendRouteComponentEncoder = enumEncoder $ \case
  Some.This BackendRoute_GetData-> Just "data"

routeComponentEncoder
  :: (MonadError Text check, MonadError Text parse)
  => Encoder check parse (Some Route) (Maybe Text)
routeComponentEncoder = enumEncoder $ \case
  Some.This Route_Home -> Nothing

routeRestEncoder
  :: (Applicative check, MonadError Text parse)
  => Route a -> Encoder check parse a PageName
routeRestEncoder = Encoder . pure . \case
  Route_Home -> endValidEncoder mempty

concat <$> mapM deriveRouteComponent
  [ ''Route
  , ''BackendRoute
  ]

