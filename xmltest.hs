--author: Jonathan Terner
--description: I'm testing the parsing of xml from the game database


--this is code from the wiki
{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}


import Text.XML.HXT.Core


data Game = Game {
		gameId :: String,
		gameTitle :: String,
		releaseYear :: String,
		platform :: String
		} deriving (Show)


getGame' = deep (isElem >>> hasName "Game") >>> proc x -> do
			id' <- getText <<< getChildren <<< deep (hasName "id") -< x
			title <- getText <<< getChildren <<< deep (hasName "GameTitle") -< x
			year <- getText <<< getChildren <<< deep (hasName "ReleaseDate") -< x
			platform' <- getText <<< getChildren <<< deep (hasName "Platform") -< x 
			returnA -< Game {gameId = id', gameTitle = title, releaseYear = year, platform = platform'}


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
  guests <- runX (readDocument [withValidate no] "text.xml" 
                    >>> getGame)
  print guests
