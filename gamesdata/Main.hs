{-# LANGUAGE OverloadedStrings #-}

import GameGet
import Database.SQLite.Simple
import Control.Applicative
import Data.Functor
import Data.Maybe

--   sqlite3 gamelist.db "CREATE TABLE pc_games (id INTEGER PRIMARY KEY, gameId INTEGER, title TEXT, year TEXT, platform TEXT, url TEXT, price REAL);"

main = do
	a <- gameAtConsole "PC"
	sequence $ take 100 $ map addGame a




addGame :: Game -> IO ()
addGame game = do
	conn <- open "gamelist.db"
   	execute conn "INSERT INTO pc_games (gameId,title,year,platform,url) VALUES (?,?,?,?,?)" (gameId game::Int, gameTitle game::String, releaseYear game::String, platform game::String, url game::String)
   	close conn


