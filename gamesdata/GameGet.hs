--author: Jonathan Terner
--description: I'm testing the parsing of xml from the game database


--this is code from the wiki
{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}


import Text.XML.HXT.Core
import Control.Applicative
import Network.Curl
import Data.Functor
import Control.Monad
import Data.Maybe

data Game = Game {
		gameId :: Int,
		gameTitle :: String,
		releaseYear :: String,
		platform :: String,
		url :: String
		} deriving (Show)

data ImgUrl = ImgUrl [String]


atTag tag = deep (isElem >>> hasName tag)
text = getChildren >>> getText

parseXML file = readDocument [ withValidate no
                             , withRemoveWS yes  -- throw away formating WS
                             ] file



getGame = atTag "Data" >>>
	proc l -> do
		baseUrl <- getBaseUrl -< l
		game <- atTag "Game" -< l
		gameId' <- text <<< atTag "id" -< game
		gameTitle' <- text <<< atTag "GameTitle" -< game
		releaseYear' <- text <<< atTag "ReleaseDate" -< game
		platform' <- text <<< atTag "Platform" -< game
		returnA -< [gameId', gameTitle',releaseYear',platform',baseUrl]



getArt = atTag "boxart" >>>
	proc l -> do
		front <- getAttrValue "thumb" -< l
		returnA -< front

getBaseUrl = atTag "Data" >>>
	proc x -> do
		url <- text <<< atTag "baseImgUrl" -< x
		returnA -< url

-- just a helper function to make creating a Game type easier
makeGame :: [[String]] -> String -> Maybe Game
makeGame [] _ = Nothing
makeGame _ [] = Nothing
makeGame ([a:b:c:d:e:_]) artUrl 	= Just $ Game (read a::Int) b c d (e ++ artUrl)
      

		
main = do
  putStrLn "Enter an id"
  x <- getLine
  response <- snd <$> curlGetString ("http://thegamesdb.net/api/GetGame.php?id="++x) []
  writeFile "text.xml" response
  games' <- runX (parseXML "text.xml" 
                    >>> getGame)

  art <- runX (parseXML "text.xml" >>> getArt)
 
  let game = makeGame (games') (head art)
  print game
