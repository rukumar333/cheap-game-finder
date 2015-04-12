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

--used by gameAtId
getGame = atTag "Data" >>>
	proc l -> do
		art <- getArt -< l
		game <- atTag "Game" -< l
		gameId' <- text <<< atTag "id" -< game
		gameTitle' <- text <<< atTag "GameTitle" -< game
		releaseYear' <- text <<< atTag "ReleaseDate" -< game
		platform' <- text <<< atTag "Platform" -< game
		returnA -< Game (read gameId'::Int) gameTitle' releaseYear' platform' ("http://thegamesdb.net/banners/" ++ art)
--used by gameAtConsole
getGame' console = atTag "Data" >>>
	proc l -> do
		game <- atTag "Game" -< l
		gameId' <- text <<< atTag "id" -< game
		gameTitle' <- text <<< atTag "GameTitle" -< game
		releaseYear' <- text <<< atTag "ReleaseDate" -< game
		art <- text <<< atTag "thumb" -< game
		returnA -< Game (read gameId'::Int) gameTitle' releaseYear' console ("http://thegamesdb.net/banners/" ++ art)


--used by gameAtId
getArt = atAttr "boxart" "side" >>>
	proc s -> do
		url <- getAttrValue "thumb" -< s
		returnA -< url






-- just a helper function to make creating a Game type easier
makeGame :: [Game] -> Maybe Game
makeGame []  = Nothing
makeGame g = case g of
		([x]) -> Just x
		(x:xs) -> Just (last xs)


		
gameAtId :: Int-> IO (Maybe Game)
gameAtId x = do
  response <- snd <$> curlGetString ("http://thegamesdb.net/api/GetGame.php?id="++(show x)) [] 
  games' <- runX (parseXML response 
                   >>> getGame)
  return $ makeGame games'


gameAtConsole :: String -> IO [Game]
gameAtConsole console = do
	response <- snd <$> curlGetString ("http://thegamesdb.net/api/PlatformGames.php?platform="++console) []
	games <- runX (parseXML response 
                   >>> getGame' console)
	return games
	


