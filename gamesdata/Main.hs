{-# LANGUAGE OverloadedStrings #-}

import GameGet
import Database.SQLite.Simple
import Control.Applicative
import Data.Functor
import Data.Maybe
import Data.Monoid
import Data.String

-- these commands need to be run to set up the data base
--   sqlite3 gamelist.db "CREATE TABLE pc_games (id INTEGER PRIMARY KEY, gameId INTEGER, title TEXT, year TEXT, platform TEXT, url TEXT, price REAL);"
--   sqlite3 gamelist.db "CREATE TABLE ps3_games (id INTEGER PRIMARY KEY, gameId INTEGER, title TEXT, year TEXT, platform TEXT, url TEXT, price REAL);"
--   sqlite3 gamelist.db "CREATE TABLE ps4_games (id INTEGER PRIMARY KEY, gameId INTEGER, title TEXT, year TEXT, platform TEXT, url TEXT, price REAL);"
--   sqlite3 gamelist.db "CREATE TABLE xbox360_games (id INTEGER PRIMARY KEY, gameId INTEGER, title TEXT, year TEXT, platform TEXT, url TEXT, price REAL);"
--   sqlite3 gamelist.db "CREATE TABLE xboxOne_games (id INTEGER PRIMARY KEY, gameId INTEGER, title TEXT, year TEXT, platform TEXT, url TEXT, price REAL);"
--   sqlite3 gamelist.db "CREATE TABLE wii_games (id INTEGER PRIMARY KEY, gameId INTEGER, title TEXT, year TEXT, platform TEXT, url TEXT, price REAL);"
--   sqlite3 gamelist.db "CREATE TABLE wiiU_games (id INTEGER PRIMARY KEY, gameId INTEGER, title TEXT, year TEXT, platform TEXT, url TEXT, price REAL);"
main = do
	a <- gameAtConsole "PC"
	conn <- open "gamelist.db"
	mapM_ (addGame (fromString "pc_games"::Query) conn) a
	close conn
	
	





-- parameters Game object and table name 

addGame table connection game = do
   	execute connection ("INSERT INTO " <> table <> " (gameId,title,year,platform,url) VALUES (?,?,?,?,?)") (gameId game::Int, gameTitle game::String, releaseYear game::String, platform game::String, url game::String)
 

-- this function will clear gamelist.db if there is already information in it
setUpDb :: IO ()
setUpDb = do
	conn <- open "gamelist.db"
	execute conn "DELETE FROM pc_games" ()
	close conn
	