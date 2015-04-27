{-# LANGUAGE OverloadedStrings #-}

module Walmart where

import Data.Aeson
import Data.Functor
import Data.Maybe
import Data.String
import Network.Curl
import Control.Applicative
import Control.Monad


data WalmartList = WalmartList
    {
      items :: [WalmartGame]
    }
                   deriving(Show)

data WalmartGame = WalmartGame
    {
      name :: String,
      price :: Double,
      productUrl :: String
    }
                   deriving(Show)

instance FromJSON WalmartList where
    parseJSON (Object v) = WalmartList
                           <$> v .: "items"

instance FromJSON WalmartGame where
    parseJSON (Object v) = WalmartGame
                           <$> v .: "name"
                           <*> v .: "salePrice"
                           <*> v .: "productUrl"                          

createSpaceQuery :: [String] -> String
createSpaceQuery platform | length platform == 1 = head platform
                             | otherwise            = take (length resultString - 3) resultString
                             where resultString = foldr (\x y -> x ++ "%20" ++ y) [] platform

getWalmart name = do
  query <- return $ createSpaceQuery (words name)
  response <- snd <$> curlGetString ("http://api.walmartlabs.com/v1/search?apiKey=cb6xua2avdqjj4ck26zry2jh&query=" ++ query) []
  let games = decode (fromString response)
  return $ fromJust (games :: Maybe WalmartList)

main = do
  -- Walmart -- 
  name <- getLine
  query <- return $ createSpaceQuery (words name)
  response <- snd <$> curlGetString ("http://api.walmartlabs.com/v1/search?apiKey=cb6xua2avdqjj4ck26zry2jh&query=" ++ query) []
  let games' = decode (fromString response)
  print . items $ fromJust $ (games' :: Maybe WalmartList)

