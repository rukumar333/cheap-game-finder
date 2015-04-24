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
    show (Game na pr pl) = "Game -> Name: " ++ na ++ ", Price: " ++ (show pr) ++ ", Platform: " ++ pl

instance Show BestBuyList where
    show (BestBuyList gs pr) = "Got list"

createBestBuyQuery :: String -> String -> String
createBestBuyQuery name platform = "(" ++ (createSearchQuery name') ++ "&platform=" ++ (createPlatformQuery platform') ++ ")"
    where name'     = words name
          platform' = words platform

createSearchQuery :: [String] -> String
createSearchQuery title = tail $ foldr (\x y -> "&search=" ++ x ++ y) [] title

createPlatformQuery :: [String] -> String
createPlatformQuery platform | length platform == 1 = head platform
                             | otherwise            = take (length resultString - 3) resultString
                             where resultString = foldr (\x y -> x ++ "%20" ++ y) [] platform

-- getBestBuyList :: String -> String -> IO ()
-- getBestBuyList name platform = do
  


main = do
  name <- getLine 
  platform <- getLine
  query <- return $ createBestBuyQuery name platform
  -- response <- snd <$> curlGetString "http://api.remix.bestbuy.com/v1/products(search=dragon&search=age&search=inquisition&platform=playstation%204)?show=name,salePrice,platform&format=json&apiKey=xdapygd5t8dwnbbn5653h9jh" []
  response <- snd <$> curlGetString ("http://api.remix.bestbuy.com/v1/products" ++ query ++ "?show=name,salePrice,platform&format=json&apiKey=xdapygd5t8dwnbbn5653h9jh") []
  print response
  let games = decode (fromString response)
  print . products $ fromJust $ (games :: Maybe BestBuyList)


-- curl 'http://api.remix.bestbuy.com/v1/products(search=dragon&search=age&search=inquisition&platform=playstation*)?show=name,salePrice,platform&format=json&apiKey=xdapygd5t8dwnbbn5653h9jh'
