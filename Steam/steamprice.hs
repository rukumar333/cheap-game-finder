{-# LANGUAGE OverloadedStrings #-}
 {-# LANGUAGE FlexibleInstances #-}
import Database.SQLite.Simple
import Control.Applicative
import Data.Functor
import Data.Maybe
import Data.Monoid
import Data.String
import Network.Curl
import Control.Monad
import Data.Aeson
import Data.Char (ord)
import Data.Text (pack)
import GHC.Int
import Text.Regex
import Steam.Steamid as S

--trim regex strings
--(\")([0-9]+[0-9]+)(\":{\"success\":true,)
--(}})


data SteamGame = SteamGame { name :: String, price::Double} deriving (Show)

fromSteamApp :: S.SteamApp -> Double -> SteamGame
fromSteamApp app price = SteamGame (S.name app) price

-- use this function from the cmd line
getGameByName :: String -> IO (Maybe SteamGame)
getGameByName name = do
	app <- S.getAppByName name
	game <- return $ getGameFromApp <$> app
	if isNothing game then return Nothing else fromJust game



getGameFromApp :: S.SteamApp -> IO (Maybe SteamGame)
getGameFromApp app = do
	response <- snd <$> curlGetString ("http://store.steampowered.com/api/appdetails?appids="++ (show $ S.appid app))[]
	let name = S.name app
	let r = matchRegex (mkRegex "(\"final\":)([0-9]+[0-9]+)") response
	let price =  (/100) <$> (fmap read (( !! 1) <$> r):: Maybe Double)
	return $ SteamGame <$> Just name <*> price
	
	
	
	
