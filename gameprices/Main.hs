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
      platform :: String
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

instance Show Game where
    show (Game na pr pl) = "Game--- Name: " ++ na ++ ", Price: " ++ (show pr) ++ ", Platform: " ++ pl

instance Show BestBuyList where
    show (BestBuyList gs pr) = "Got list"


main = do
  response <- snd <$> curlGetString "http://api.remix.bestbuy.com/v1/products(search=dragon&search=age&search=inquisition&platform=playstation%204)?show=name,salePrice,platform&format=json&apiKey=xdapygd5t8dwnbbn5653h9jh" []
  print response
  let games = decode (fromString response)
  -- print games
  -- print . fromJust $ (games :: Maybe BestBuyList)
  print . products $ fromJust $ (games :: Maybe BestBuyList)
  -- print . fromJust $ ( :: Maybe [Game])


-- curl 'http://api.remix.bestbuy.com/v1/products(search=dragon&search=age&search=inquisition&platform=playstation*)?show=name,salePrice,platform&format=json&apiKey=xdapygd5t8dwnbbn5653h9jh'
