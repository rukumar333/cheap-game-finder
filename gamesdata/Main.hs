{-# LANGUAGE OverloadedStrings #-}

import GameGet
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
--   sqlite3 gamelist.db "CREATE TABLE wiiU_games (id INTEGER PRIMARY KEY, gameId INTEGER, title TEXT, year TEXT, platform TEXT, url TEXT, price REAL);"


instance FromRow Game where
	fromRow = Game <$> field <*> field <*> field <*> field <*> field

instance ToRow Game where
	toRow game = [SQLInteger (fromIntegral $ gameId game), SQLText (pack $ gameTitle game), SQLText (pack $ releaseYear game), SQLText (pack $ platform game), SQLText (pack $ url game)] 

main = do
	a <- gameAtConsole "PC"
	conn <- open "gamelist.db"
	mapM_ (addGame (fromString "pc_games"::Query) conn) a
	close conn
	
	




-- parameters Game object and table name 

addGame table connection game = do
   	execute connection ("INSERT INTO " <> table <> " (gameId,title,year,platform,url) VALUES (?,?,?,?,?)") game


 


--removes games that are older than the specified paramater (in years)

	
