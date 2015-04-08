--author: Jonathan Terner
--description: I'm testing the parsing of xml from the game database


--this is code from the wiki
import Text.XML.HXT.Core


data Game = Game {
		gameId :: Int,
		gameTitle :: String,
		releaseYear :: Int,
		platform :: String
		} deriving (Show)






