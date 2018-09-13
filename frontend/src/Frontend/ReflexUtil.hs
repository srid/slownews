{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Frontend.ReflexUtil
  ( getAndDecodeWithError
  ) where

import Control.Monad (sequence)
import Control.Monad.Fix (MonadFix)
import Data.Aeson (FromJSON, eitherDecode)
import Data.Bifunctor (Bifunctor (bimap))
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe)
import Data.Text as T
import Data.Text.Encoding (encodeUtf8)

import Reflex.Dom hiding (Link, mainWidgetWithCss, run)

-- | A version of `getAndDecode` that handles JSON error in either monad. Also
-- converts XhrException to String for consistency.
getAndDecodeWithError
  :: (FromJSON a, MonadWidget t m)
  => Event t Text
  -> m (Event t (Either String a))
getAndDecodeWithError url = do
  r <- performRequestAsyncWithError $ fmap (\x -> XhrRequest "GET" x def) url
  return $ fmap (\e -> bimap show id e >>= decodeXhrResponseWithError) r

-- | Convenience function to decode JSON-encoded responses.
decodeXhrResponseWithError :: FromJSON a => XhrResponse -> Either String a
decodeXhrResponseWithError =
  fromMaybe (Left "Empty response") . sequence
  . traverse (eitherDecode . BL.fromStrict . encodeUtf8)
  . _xhrResponse_responseText
