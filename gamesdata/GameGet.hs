{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}


import Text.XML.HXT.Core
import Control.Applicative
import Network.Curl
import Data.Functor
import Data.Monoid


data Game = Game {
	gameid :: Int,
	title :: String,
	platform :: String,
	coverArtUrl :: String,
	} deriving (Show)
	
atTag tag = deep (isElem >>> hasName tag)
text = getChildren >>> getText

getGame = atTag "Game" >>>
	proc x -> do
		gameId' <- text <<< atTag "id" -< x
		gameTitle' <- text <<< atTag "GameTitle" -< x
		platform' <- text <<< atTag "Platform" -< x
		baseUrl <- text <<< atTag "baseImgUrl" -< x
		ftArt <- text <<< atTag "boxart
		returnA -< Game {gameId = (read gameId'::Int), title = gameTitle', platform = platform', coverArtUrl = baseUrl <> }
