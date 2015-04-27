{-# LANGUAGE OverloadedStrings #-}

module BestBuy where

import Data.Aeson
import Data.Functor
import Data.Maybe
import Data.String
import Network.Curl
import Control.Applicative
import Control.Monad

data BestBuyList = BestBuyList
    { 
      total :: Int,     
      products :: [Game]
    }
                   deriving (Show)

data Game = Game
    {
      name :: String,
      price :: Double,
      platform :: String,
      productUrl :: String
    }
            deriving (Show)


instance FromJSON BestBuyList where
    parseJSON (Object v) = BestBuyList 
                           <$> v .: "total"
                           <*> v .: "products"

instance FromJSON Game where
    parseJSON (Object v) = Game 
                           <$> v .: "name"
                           <*> v .: "salePrice"
                           <*> v .: "platform"
                           <*> v .: "url"

createBestBuyQuery :: String -> String -> String
createBestBuyQuery name platform = "(" ++ (createSearchQuery name') ++ "&platform=" ++ (createSpaceQuery platform') ++ ")"
    where name'     = words name
          platform' = words platform

createSearchQuery :: [String] -> String
createSearchQuery title = tail $ foldr (\x y -> "&search=" ++ x ++ y) [] title

createSpaceQuery :: [String] -> String
createSpaceQuery platform | length platform == 1 = head platform
                             | otherwise            = take (length resultString - 3) resultString
                             where resultString = foldr (\x y -> x ++ "%20" ++ y) [] platform


getBestBuy name platform = do
  query <- return $ createBestBuyQuery name platform           
  response <- snd <$> curlGetString ("http://api.remix.bestbuy.com/v1/products" ++ query ++ "?show=name,salePrice,platform,url&format=json&apiKey=xdapygd5t8dwnbbn5653h9jh") []                
  let games = decode (fromString response)
  return $ fromJust (games :: Maybe BestBuyList)

main = do
  -- Best Buy -- 
  name <- getLine 
  platform <- getLine
  query <- return $ createBestBuyQuery name platform
  response <- snd <$> curlGetString ("http://api.remix.bestbuy.com/v1/products" ++ query ++ "?show=name,salePrice,platform,url&format=json&apiKey=xdapygd5t8dwnbbn5653h9jh") []                
  let games = decode (fromString response)
  print . products $ fromJust $ (games :: Maybe BestBuyList)


