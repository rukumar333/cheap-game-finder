{-# LANGUAGE OverloadedStrings #-}

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

getUrl n  = do
	conn <- open "games.db"
	games <- query_ conn ("SELECT gameId, title, year, platform, url FROM games  WHERE title = '" <> (fromString n ::Query) <>"';") :: IO [Game]
        
	print $ url $ Prelude.head games


 


--removes games that are older than the specified paramater (in years)

	
