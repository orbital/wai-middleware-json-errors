{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Middleware.JsonErrors
  ( jsonErrors
  ) where

import Data.Aeson (Value(..), object, (.=), encode)
import Data.Text.Encoding (decodeUtf8)
import Data.List (lookup)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (ByteString, toStrict)
import Data.Binary.Builder (fromLazyByteString, toLazyByteString)
import Network.HTTP.Types.Status (Status(statusCode))
import Network.HTTP.Types.Header (ResponseHeaders)
import Network.Wai (Application, Response, modifyResponse, responseStatus, responseHeaders, responseBuilder)
import Network.Wai.Internal (Response(..))

jsonErrors :: Application -> Application
jsonErrors = modifyResponse responseModifier


responseModifier :: Response -> Response
responseModifier r =
  case errorInfo r of
    Nothing -> r
    Just (s, hs, b) ->
      jsonErrorResponse s hs b


jsonErrorResponse :: Status -> ResponseHeaders -> ByteString -> Response
jsonErrorResponse s hs b =
    responseBuilder s (("Content-Type", "application/json") : hs) $
      fromLazyByteString $ encode $ object
        [ "error" .= String (decodeUtf8 $ toStrict b)
        , "status" .= Number (fromIntegral $ statusCode s)
        ]


responseBody :: Response -> Maybe ByteString
responseBody (ResponseBuilder _ _ b) = Just (toLazyByteString b)
responseBody (ResponseRaw _ r) = responseBody r
responseBody (ResponseFile _ _ _ _) = Nothing
responseBody (ResponseStream _ _ _) = Nothing


isPlainTextError :: Status -> ResponseHeaders -> Bool
isPlainTextError s hs =
     statusCode s >= 400
  && not (isContentType "application/json" hs)


errorInfo :: Response -> Maybe (Status, ResponseHeaders, ByteString)
errorInfo r =
    let s = responseStatus r
        hs = responseHeaders r
        mb = responseBody r
    in if isPlainTextError s hs
      then (s, hs,) <$> mb
      else Nothing


isContentType :: BS.ByteString -> ResponseHeaders -> Bool
isContentType b hs =
    lookup "Content-Type" hs == Just b
