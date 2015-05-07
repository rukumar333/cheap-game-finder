{-# LANGUAGE OverloadedStrings #-}

module Prices.BestBuy where

import Data.Aeson
import Data.Functor
import Data.Maybe
import Data.String
import qualified Data.Text as D
import Network.Curl
import Control.Applicative
import Control.Monad

data BestBuyList = BestBuyList
    { 
      products :: [Game]
    }
            deriving (Show)

data Game = Game
    {
      name :: D.Text,
      price :: Double,
      platform :: D.Text,
      productUrl :: D.Text,
      frontImage :: D.Text
    }
            deriving (Show)


instance FromJSON BestBuyList where
    parseJSON (Object v) = BestBuyList 
                           <$> v .: "products"

instance FromJSON Game where
    parseJSON (Object v) = Game 
                           <$> v .: "name"
                           <*> v .: "salePrice"
                           <*> v .: "platform"
                           <*> v .: "url"
                           <*> v .: "largeFrontImage"

checkInputBestBuy :: D.Text -> Game -> Bool
checkInputBestBuy input game = D.isInfixOf (D.toLower input) (D.toLower $ name game)

createIOBestBuyGames :: IO BestBuyList -> IO [Game]
createIOBestBuyGames list = products <$> list

bestBuyGameFilter :: IO [Game] -> D.Text -> IO [Game]
bestBuyGameFilter list nm = (<$>) ((filter (\x -> (checkHasPlatformName $ name x) && (checkInputBestBuy nm x)))) list

filterBestBuy :: IO BestBuyList -> D.Text ->  IO [Game]
filterBestBuy list nm = bestBuyGameFilter (createIOBestBuyGames list) nm

fixList :: IO BestBuyList -> D.Text -> IO [Game]
fixList list nm = fixWindowsPlatformMap $ (map (\x -> Game (removePlatformName $ name x) (price x) (platform x) (productUrl x) (frontImage x))) <$> (filterBestBuy list nm)

fixWindowsPlatform :: Game -> Game
fixWindowsPlatform game = Game (name game) (price game) pl (productUrl game) (frontImage game)
    where pl | (platform game) == "Windows" = "PC"
             | otherwise                    = platform game

fixWindowsPlatformMap :: IO [Game] -> IO [Game]
fixWindowsPlatformMap list = (<$>) (map fixWindowsPlatform) list

checkHasPlatformName :: D.Text -> Bool  
checkHasPlatformName gn | D.isInfixOf " - PlayStation 3" gn = True 
                        | D.isInfixOf " - PlayStation 4" gn = True
                        | D.isInfixOf " - Xbox 360" gn = True 
                        | D.isInfixOf " - Xbox One" gn = True 
                        | D.isInfixOf " - Wii" gn = True 
                        | D.isInfixOf " - Wii U" gn = True 
                        | D.isInfixOf " - Windows" gn = True 
                        | D.isInfixOf " - PC" gn = True 
                        | D.isInfixOf " PC" gn = True 
                        | otherwise = False

removePlatformName :: D.Text -> D.Text
removePlatformName gn | D.isInfixOf " - PlayStation 3" gn = D.replace " - PlayStation 3" "" gn
                      | D.isInfixOf " - PlayStation 4" gn = D.replace " - PlayStation 4" "" gn
                      | D.isInfixOf " - Xbox 360" gn = D.replace " - Xbox 360" "" gn
                      | D.isInfixOf " - Xbox One" gn = D.replace " - Xbox One" "" gn
                      | D.isInfixOf " - Wii" gn = D.replace " - Wii" "" gn
                      | D.isInfixOf " - Wii U" gn = D.replace " - Wii U" "" gn
                      | D.isInfixOf " - Windows" gn = D.replace " - Windows" "" gn
                      | D.isInfixOf " - PC" gn = D.replace " - PC" "" gn
                      | D.isInfixOf " PC" gn = D.replace " PC" "" gn
                      | otherwise = gn

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
  query <- return $ createBestBuyQuery name platform'
  response <- snd <$> curlGetString ("http://api.remix.bestbuy.com/v1/products" ++ query ++ "?show=name,salePrice,platform,url,largeFrontImage&format=json&apiKey=xdapygd5t8dwnbbn5653h9jh") []                
  let games = decode (fromString response)
  fixList (return $ fromJust (games :: Maybe BestBuyList)) (D.pack name)
                           where platform' | platform == "PC" = "windows"
                                           | otherwise        = platform

main = do
  -- Best Buy -- 
  name <- getLine 
  platform <- getLine
  query <- return $ createBestBuyQuery name platform
  response <- snd <$> curlGetString ("http://api.remix.bestbuy.com/v1/products" ++ query ++ "?show=name,salePrice,platform,url,largeFrontImage&format=json&apiKey=xdapygd5t8dwnbbn5653h9jh") []                
  let games = decode (fromString response)
  print . products $ fromJust $ (games :: Maybe BestBuyList)


