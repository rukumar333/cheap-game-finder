{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Lucid
import Lucid.Base
import Data.Monoid
import Data.Text.Internal
import Control.Applicative
import Control.Monad
import Data.String
import Web.Scotty
import qualified Prices.Walmart as W 
import qualified Prices.BestBuy as B

main :: IO ()
main = scotty 3000 $ do
         -- scriptFile <- readFile "/bootstrap/js/bootstrap.min.js"
         get "/css/index.css" $ file "css/index.css"
         get "/bootstrap/css/bootstrap.min.css" $ file "bootstrap/css/bootstrap.min.css"
         get "/bootstrap/js/bootstrap.min.js" $ file "bootstrap/js/bootstrap.min.js"
         get "/res/background.jpg" $ file "res/background.jpg"
         get "/" $ file "./index.html"

         get "/search" $
             
             html . renderText $
             -- doctype_ html_ $ do
                  html_ [lang_ "en"] $ do 
                       head_ $ do
                         meta_ [charset_ "utf-8"]
                         meta_ [httpEquiv_ "X-UA-Compatible", content_ "IE-edge"]
                         title_ "Game Finder"
                         link_ [href_ "bootstrap/css/bootstrap.min.css", rel_ "stylsheet", type_ "text/css"]
                         link_ [href_ "css/index.css", rel_ "stylsheet", type_ "text/css"]
                       body_ $ do
                         -- h1_ "Submitted data"
                         nav_ [class_ "navbar navbar-inverse"] $ do
                                         div_ [class_ "container-fluid"] $ do
                                                                   div_ [class_ "navbar-header"] $ do
                                                                             a_ [class_ "navbar-brand", href_ "#"] "Cheap Game Finder"
                         div_ [class_ "background"] ""
                         -- script_ $ src_ "https://ajax.googleapis.com/ajax/libs/jquery/1.11.2/jquery.min.js"
                         -- with script_ [src_ "/bootstrap/js/bootstrap.min.js"] 
                         -- scriptFile <- readFile "/bootstrap/js/bootstrap.min.js"
                         script_ $ do
                                 -- scriptFile <- readFile "/bootstrap/js/bootstrap.min.js"
                                 fromString scriptFile
                         -- h1_ "Hello"
                         -- print scriptFile
                         -- script_ (fromString scriptFile)

         
                     
