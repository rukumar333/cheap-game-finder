--author: Jonathan Terner
--description: I'm testing the parsing of xml from the game database


--this is code from the wiki
{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}


import Text.XML.HXT.Core
import Control.Applicative
import Network.Curl
import Data.Functor

data Game = Game {
		gameId :: String,
		gameTitle :: String,
		releaseYear :: String,
		platform :: String
		} deriving (Show)


atTag tag = deep (isElem >>> hasName tag)
text = getChildren >>> getText


getGame = atTag "Game" >>>
	proc x -> do
		gameId' <- text <<< atTag "id" -< x
		gameTitle' <- text <<< atTag "GameTitle" -< x
		releaseYear' <- text <<< atTag "ReleaseDate" -< x
		platform' <- text <<< atTag "Platform" -< x
		returnA -< Game {gameId = gameId', gameTitle = gameTitle', releaseYear = releaseYear', platform = platform'}

main = do
  putStrLn "Enter a search term"
  x <- getLine
  response <- snd <$> curlGetString ("http://thegamesdb.net/api/GetGamesList.php?name=" ++ x) []
  writeFile "text.xml" response
  guests <- runX (readDocument [withValidate no] "text.xml" 
                    >>> getGame)
  
  print guests
