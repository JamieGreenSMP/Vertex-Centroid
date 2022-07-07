module Main where
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as BC
import Network.URI.Encode
import Network.HTTP.Simple



myToken :: BC.ByteString
myToken = "52e015ce286a4a1a89cb106a3f30e26e"

openCageHost :: BC.ByteString
openCageHost = "api.opencagedata.com"

locationToUriLocation :: String -> BC.ByteString
locationToUriLocation location = BC.pack $ encode location

locationPath :: BC.ByteString -> BC.ByteString
locationPath uriLocation = BC.pack "/geocode/v1/geojson?q=" <> uriLocation <> BC.pack "&key=" <> myToken <> BC.pack "&key="

buildRequest :: BC.ByteString -> BC.ByteString -> BC.ByteString
             -> BC.ByteString -> Request
buildRequest token host method path  = setRequestMethod method
                                  $ setRequestHost host
                                  $ setRequestHeader "token" [token]
                                  $ setRequestPath path
                                  $ setRequestSecure True
                                  $ setRequestPort 443
                                  $ defaultRequest

locationRequest :: String -> Request
locationRequest location = buildRequest myToken openCageHost "GET" $ locationPath $ locationToUriLocation location

-- getLatLongFromLocation :: String -> LatLong
getLatLongFromLocation location = do
    response <- httpLbs $ locationRequest location
    decode <$> BC.unpack response



main :: IO ()
main = print $ getLatLongFromLocation "25 Deacon Road" 