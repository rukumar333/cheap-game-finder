{-# LANGUAGE OverloadedStrings #-}

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

data Game = Game
    {
      name :: String,
      price :: Double,
      platform :: String,
      productUrl :: String
    }

data WalmartList = WalmartList
    {
      items :: [WalmartGame]
    }

data WalmartGame = WalmartGame
    {
      name' :: String,
      price' :: Double,
      productUrl' :: String
    }

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

instance FromJSON WalmartList where
    parseJSON (Object v) = WalmartList
                           <$> v .: "items"

instance FromJSON WalmartGame where
    parseJSON (Object v) = WalmartGame
                           <$> v .: "name"
                           <*> v .: "salePrice"
                           <*> v .: "productUrl"
                           

instance Show Game where
    show (Game na pr pl ul) = "Game -> Name: " ++ na ++ ", Price: " ++ (show pr) ++ ", Platform: " ++ pl ++ ", url: " ++ ul

instance Show WalmartGame where
    show (WalmartGame na pr ul) = "Game -> Name: " ++ na ++ ", Price: " ++ (show pr) ++ ", " ++ ul

instance Show BestBuyList where
    show (BestBuyList gs pr) = "Got list"

createBestBuyQuery :: String -> String -> String
createBestBuyQuery name platform = "(" ++ (createSearchQuery name') ++ "&platform=" ++ (createSearchQuery platform') ++ ")"
    where name'     = words name
          platform' = words platform

createSearchQuery :: [String] -> String
createSearchQuery title = tail $ foldr (\x y -> "&search=" ++ x ++ y) [] title

createSpaceQuery :: [String] -> String
createSpaceQuery platform | length platform == 1 = head platform
                             | otherwise            = take (length resultString - 3) resultString
                             where resultString = foldr (\x y -> x ++ "%20" ++ y) [] platform

-- getBestBuyList :: String -> String -> IO ()
-- getBestBuyList name platform = do



main = do
  -- Best Buy -- 
  -- name <- getLine 
  -- platform <- getLine
  -- query <- return $ createBestBuyQuery name platform
  -- response <- snd <$> curlGetString ("http://api.remix.bestbuy.com/v1/products" ++ query ++ "?show=name,salePrice,platform,url&format=json&apiKey=xdapygd5t8dwnbbn5653h9jh") []                
  -- let games = decode (fromString response)
  -- print . products $ fromJust $ (games :: Maybe BestBuyList)

  -- Walmart -- 
  name <- getLine
  query <- return $ createSpaceQuery (words name)
  response <- snd <$> curlGetString ("http://api.walmartlabs.com/v1/search?apiKey=cb6xua2avdqjj4ck26zry2jh&query=" ++ query) []
  let games' = decode (fromString response)
  print . items $ fromJust $ (games' :: Maybe WalmartList)

