{-# LANGUAGE OverloadedStrings #-}

import GameGet
import Database.SQLite.Simple
import Control.Applicative
import Data.Functor
import Data.Maybe



main = do
	a <- mapM gameAtId [1..100]
	let b = map fromJust $ filter (not. isNothing) a
	sequence $ map addGame b
	print $ pcFilter b



addGame :: Game -> IO ()
addGame game = do
	conn <- open "gamelist.db"
   	execute conn "INSERT INTO pc_games (gameId,title,year,platform,url) VALUES (?,?,?,?,?)" (gameId game::Int, gameTitle game::String, releaseYear game::String, platform game::String, url game::String)
   	close conn

pcFilter:: [Game] -> [Game]
pcFilter games = filter (\x -> (platform x) == "PC") games
