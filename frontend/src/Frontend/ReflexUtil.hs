{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Frontend.ReflexUtil
  ( getAndDecodeWithError
  , matchMaybe
  , matchEither) where

import Control.Monad (sequence)
import Data.Aeson (FromJSON, eitherDecode)
import Data.Bifunctor (Bifunctor (bimap))
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe)
import Data.Text as T
import Data.Text.Encoding (encodeUtf8)

import Reflex.Dom hiding (Link, mainWidgetWithCss, run)

-- | A version of `getAndDecode` that handles JSON error in either monad. Also
-- converts XhrException to String for consistency.
getAndDecodeWithError :: (FromJSON a, MonadWidget t m)
                      => Event t Text
                      -> m (Event t (Either String a))
getAndDecodeWithError url = do
  r <- performRequestAsyncWithError $ fmap (\x -> XhrRequest "GET" x def) url
  return $ fmap (\e -> bimap show id e >>= decodeXhrResponseWithError) r

-- | Convenience function to decode JSON-encoded responses.
decodeXhrResponseWithError :: FromJSON a => XhrResponse -> Either String a
decodeXhrResponseWithError =
  eitherMaybeHandle "Empty response"
  . traverse (eitherDecode . BL.fromStrict . encodeUtf8)
  . _xhrResponse_responseText

eitherMaybeHandle :: a -> Either a (Maybe b) -> Either a b
eitherMaybeHandle err = fromMaybe (Left err) . sequence


-- | These functions exist to simplify nested either/maybe data types.

matchMaybe :: MonadWidget t m
           => Dynamic t (Maybe a)
           -> (Maybe (Dynamic t a) -> m b)
           -> m ()
matchMaybe m f = do
  v <- maybeDyn m
  dyn_ $ ffor v f

matchEither :: MonadWidget t m
            => Dynamic t (Either a b)
            -> (Either (Dynamic t a) (Dynamic t b) -> m c)
            -> m ()
matchEither e f = do
  v <- eitherDyn e
  dyn_ $ ffor v f
