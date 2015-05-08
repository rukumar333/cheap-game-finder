{-# LANGUAGE OverloadedStrings #-}
module Gamesdata.GamesDB where
import Gamesdata.GameGet
import Database.SQLite.Simple
import Control.Applicative
import Data.Functor
import Data.Maybe
import Data.Monoid
import Data.String
import GHC.Int
import Data.Text
import Data.Time.Clock
import Data.Time.Calendar
import Data.Char (ord)
import Resolve

-- these commands need to be run to set up the data base
--   sqlite3 gamelist.db "CREATE TABLE pc_games (id INTEGER PRIMARY KEY, gameId INTEGER, title TEXT, year TEXT, platform TEXT, url TEXT, price REAL);"
--   sqlite3 gamelist.db "CREATE TABLE ps3_games (id INTEGER PRIMARY KEY, gameId INTEGER, title TEXT, year TEXT, platform TEXT, url TEXT, price REAL);"
--   sqlite3 gamelist.db "CREATE TABLE ps4_games (id INTEGER PRIMARY KEY, gameId INTEGER, title TEXT, year TEXT, platform TEXT, url TEXT, price REAL);"
--   sqlite3 gamelist.db "CREATE TABLE xbox360_games (id INTEGER PRIMARY KEY, gameId INTEGER, title TEXT, year TEXT, platform TEXT, url TEXT, price REAL);"
--   sqlite3 gamelist.db "CREATE TABLE xboxOne_games (id INTEGER PRIMARY KEY, gameId INTEGER, title TEXT, year TEXT, platform TEXT, url TEXT, price REAL);"
--   sqlite3 gamelist.db "CREATE TABLE wii_games (id INTEGER PRIMARY KEY, gameId INTEGER, title TEXT, year TEXT, platform TEXT, url TEXT, price REAL);"
--   sqlite3 gamelist.db "CREATE TABLE wiiU_games (id INTEGER PRIMARY KEY, gameId INTEGER, title TEXT, year TEXT, platform TEXT, url TEXT);"



instance FromRow Game where
	fromRow = Game <$> field <*> field <*> field <*> field <*> field

instance ToRow Game where
	toRow game = [SQLInteger (fromIntegral $ gameId game), SQLText (pack $ gameTitle game), SQLText (pack $ releaseYear game), SQLText (pack $ platform game), SQLText (pack $ url game)] 

main = do
	a <- gameAtConsole "PC"
	b <- gameAtConsole "Sony+Playstation+3"
	c <- gameAtConsole "Sony+Playstation+4"
	d <- gameAtConsole "Microsoft+Xbox+360"
	e <- gameAtConsole "Microsoft+Xbox+One"
	f <- gameAtConsole "Nintendo+Wii"
	g <- gameAtConsole "Nintendo+Wii+U"
	
	conn <- open "games.db"
	execute_ conn "BEGIN;"
	mapM_ (addGame  conn) a
        mapM_ (addGame  conn) b
        mapM_ (addGame  conn) c
        mapM_ (addGame  conn) d
        mapM_ (addGame  conn) e
        mapM_ (addGame  conn) f
        mapM_ (addGame  conn) g
	execute_ conn "COMMIT;"
	close conn
        print $ (Prelude.length a)+(Prelude.length b)+(Prelude.length c)+(Prelude.length d)+(Prelude.length e)+(Prelude.length f)+(Prelude.length g)
	
	




-- parameters Game object and table name 

addGame connection = do
   	execute connection ("INSERT INTO games (gameId,title,year,platform,url) VALUES (?,?,?,?,?)")
--use this function to get the box art for a game
getUrl n platform  = do
	conn <- open "games.db"
	games <-  query_ conn (createQuery n (formatPlatform platform)) :: IO [Game]
	close conn
	if Prelude.null games then return ""
	else do	
		let games' = ignoreDLC games
		return $ url games'

ignoreDLC :: [Game] -> Game
ignoreDLC [x] = x
ignoreDLC games = Prelude.foldr comp (Prelude.head games) games
		where comp :: Game -> Game -> Game
		      comp a b = if (Prelude.length $ gameTitle a) < (Prelude.length $ gameTitle b) then a else b


--creates a regex string to be used when pulling a game from the database
createDBRegex :: String -> String
createDBRegex s = let list = Data.String.words (keepLetters s)
                  in  create 0 (Prelude.length list) list
  where create :: Int -> Int -> [String] -> String
	create _  _ [x] = "(" ++ x ++ ")"
        create index size (x:xs)  | index == (size - 1) = "(" ++ x ++ ")"
                                  | otherwise = "(" ++ x ++ ").+" ++ create (index + 1) size xs
        keepLetters string =  Prelude.filter (\x -> (ord x) <= 122) string



createQuery:: String -> String -> Query
createQuery n platform	= createQuery' $ (isolate . Prelude.filter (\x -> not $ x `elem` ".-:–")) n
		where createQuery' tokens = 
			let  start 	= "SELECT gameId, title, year, platform, url FROM games WHERE platform = '" <> (fromString platform::Query) <> "' AND title LIKE '%" <> (fromString (Prelude.head tokens)::Query) <> "%'"::Query
			     addtoken 	= (\x -> " AND title LIKE '%" <> (fromString x::Query) <> "%'"::Query)
			in  start <> (mconcat $ Prelude.map addtoken (Prelude.tail tokens)) <> ";"



--this function converts platform names to match options from user input on website

formatPlatform :: String -> String
formatPlatform platform | platform == "PC" = "PC"
                        | platform == "PlayStation 3" ="Sony+Playstation+3"
                        | platform == "PlayStation 4" ="Sony+Playstation+4"
                        | platform == "Xbox 360" = "Microsoft+Xbox+360"
                        | platform == "Xbox One" = "Microsoft+Xbox+One"
                        | platform == "Wii" = "Nintendo+Wii"
                        | platform == "Wii U" = "Nintendo+Wii+U"


