{-# LANGUAGE OverloadedStrings #-}

module Prices.Walmart where

import Data.Aeson
import Data.Functor
import Data.Maybe
import Data.String
import qualified Data.Text as D
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
      name :: D.Text,
      price :: Double,
      productUrl :: D.Text
    }
                   deriving(Show)

data Game = Game 
    {
      name' :: D.Text,
      price' :: Double,
      platform' :: D.Text,
      productUrl' :: D.Text
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

checkInputWalmart :: D.Text -> WalmartGame -> Bool
checkInputWalmart input game = D.isInfixOf (D.toLower input) (D.toLower $ name game)

createIOWalmartGames :: IO WalmartList -> IO [WalmartGame]
createIOWalmartGames list = items <$> list

walmartGameFilter :: IO [WalmartGame] -> D.Text -> IO [WalmartGame]
walmartGameFilter list nm = (<$>) ((filter (\x -> (checkHasPlatformName $ name x) && (checkInputWalmart nm x)))) list

filterWalmart :: IO WalmartList -> D.Text -> IO [WalmartGame]
filterWalmart list nm = walmartGameFilter (createIOWalmartGames list) nm

walmartGameToGame :: WalmartGame -> Game
walmartGameToGame game = Game (name game) (price game) plat (productUrl game)
    where plat | D.isInfixOf "(PS3)" (name game) = "PlayStation 3"
               | D.isInfixOf "(PS4)" (name game) = "PlayStation 4"
               | D.isInfixOf "(Xbox One)" (name game) = "Xbox One"
               | D.isInfixOf "(Xbox 360)" (name game) = "Xbox 360"
               | D.isInfixOf "(Wii)" (name game) = "Wii"
               | D.isInfixOf "(Wii U)" (name game) = "Wii U"
               | D.isInfixOf "(PC)" (name game) = "PC"
               | D.isInfixOf "(Xbox 360 / PS3 / PC)" (name game) = "Xbox 360 / PS3 / PC"
               | otherwise = "Error"

fixMultiplePlatforms :: Game -> D.Text -> Game
fixMultiplePlatforms game pl = Game (name' game) (price' game) plat (productUrl' game)
    where plat | (platform' game) == "Xbox 360 / PS3 / PC" = pl
               | otherwise                              = (platform' game)


filterMultiplePlatforms :: D.Text -> IO [Game] -> IO [Game]
filterMultiplePlatforms plat list = filterPlatforms plat $ (<$>) (map (\x -> fixMultiplePlatforms x plat)) list

fixList :: IO WalmartList -> D.Text -> D.Text -> IO [Game]
fixList list nm pl = filterMultiplePlatforms pl $ (map (\x -> Game (removePlatformName $ name' x) (price' x) (platform' x) (productUrl' x))) <$> ((map walmartGameToGame) <$> (filterWalmart list nm))

filterPlatforms :: D.Text -> IO [Game] -> IO [Game]
filterPlatforms pl list = (filter (\x -> (D.toLower $ platform' x) == (D.toLower pl))) <$> list

checkHasPlatformName :: D.Text -> Bool  
checkHasPlatformName gn | D.isInfixOf "(PS3)" gn = True
                        | D.isInfixOf "(PS4)" gn = True
                        | D.isInfixOf "(Xbox One)" gn = True
                        | D.isInfixOf "(Xbox 360)" gn = True
                        | D.isInfixOf "(Wii)" gn = True
                        | D.isInfixOf "(Wii U)" gn = True
                        | D.isInfixOf "(PC)" gn = True
                        | otherwise = False

removePlatformName :: D.Text -> D.Text
removePlatformName gn | D.isInfixOf " (PS3)" gn = D.replace " (PS3)" "" gn
                      | D.isInfixOf " (PS4)" gn = D.replace " (PS4)" "" gn
                      | D.isInfixOf " (Xbox One)" gn = D.replace " (Xbox One)" "" gn 
                      | D.isInfixOf " (Xbox 360)" gn = D.replace " (Xbox 360)" "" gn
                      | D.isInfixOf " (Wii)" gn = D.replace " (Wii)" "" gn
                      | D.isInfixOf " (Wii U)" gn = D.replace " (Wii U)" "" gn
                      | D.isInfixOf " (PC)" gn = D.replace " (PC)" "" gn
                      | otherwise = gn



createSpaceQuery :: [String] -> String
createSpaceQuery platform | length platform == 1 = head platform
                             | otherwise            = take (length resultString - 3) resultString
                             where resultString = foldr (\x y -> x ++ "%20" ++ y) [] platform

getWalmart name platform = do
  query <- return $ createSpaceQuery (words name)
  response <- snd <$> curlGetString ("http://api.walmartlabs.com/v1/search?apiKey=cb6xua2avdqjj4ck26zry2jh&query=" ++ query) []
  let games = decode (fromString response)
  fixList (return $ fromJust (games :: Maybe WalmartList)) (D.pack name) (D.pack platform)
  -- return $ fromJust (games :: Maybe WalmartList)

main = do
  -- Walmart -- 
  name <- getLine
  query <- return $ createSpaceQuery (words name)
  response <- snd <$> curlGetString ("http://api.walmartlabs.com/v1/search?apiKey=cb6xua2avdqjj4ck26zry2jh&query=" ++ query) []
  let games' = decode (fromString response)
  print . items $ fromJust $ (games' :: Maybe WalmartList)

