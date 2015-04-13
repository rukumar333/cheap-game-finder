--author: Jonathan Terner
--description: I'm testing the parsing of xml from the game database

--this is code from the wiki
{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

module GameGet where

import Text.XML.HXT.Core
import Control.Applicative
import Network.Curl
import Data.Functor
import Control.Monad
import Data.Maybe
import Data.Monoid
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class



data Game = Game {
		gameId :: Int,
		gameTitle :: String,
		releaseYear :: String,
		platform :: String,
		url :: String
		} deriving (Show)

data ImgUrl = ImgUrl {
		frontart::String,
		backart::String
		} deriving (Show)


atTag tag = deep (isElem >>> hasName tag)
atAttr tag a = deep (isElem >>> hasName tag >>> hasAttr a)
text = getChildren >>> getText


parseXML text = readString [ withValidate no
                             , withRemoveWS yes  -- throw away formating WS
                             ] text


getGame = atTag "Data" >>>
	proc l -> do
		baseUrl <- getBaseUrl -< l
		art <- getArt' -< l
		game <- atTag "Game" -< l
		
		gameId' <- text <<< atTag "id" -< game
		gameTitle' <- text <<< atTag "GameTitle" -< game
		releaseYear' <- text <<< atTag "ReleaseDate" -< game
		platform' <- text <<< atTag "Platform" -< game

		returnA -< [gameId', gameTitle',releaseYear',platform',baseUrl, art]


getArt' = atAttr "boxart" "side" >>>
	proc s -> do
		url <- getAttrValue "thumb" -< s
		returnA -< url

getBaseUrl = atTag "Data" >>>
	proc x -> do
		url <- text <<< atTag "baseImgUrl" -< x
		returnA -< url

-- just a helper function to make creating a Game type easier
makeGame :: [String] -> Maybe Game
makeGame []  = Nothing
makeGame (a:b:c:d:e:f:_)	= Just $ Game (read a::Int) b c d (e ++ f)


		
gameAtId :: Int-> IO (Maybe Game)
gameAtId x = do
  response <- snd <$> curlGetString ("http://thegamesdb.net/api/GetGame.php?id="++(show x)) [] 
  games' <- runX (parseXML response 
                   >>> getGame)
  if games' == []
  then do 
	return (Nothing)
  else do 
	if (length games') == 1
	then do
		return $ makeGame (games' !! 0)
	else do		 	 
		return $ makeGame (games' !! 1)

