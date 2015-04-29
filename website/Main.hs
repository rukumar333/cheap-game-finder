{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Lucid
import Lucid.Base
import Data.Monoid
import Data.Text.Internal
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.String
import Web.Scotty
import qualified Prices.Walmart as W 
import qualified Prices.BestBuy as B

main :: IO ()
main = scotty 3000 $ do
         get "/css/index.css" $ file "website/css/index.css"
         get "/bootstrap/css/bootstrap.min.css" $ file "website/bootstrap/css/bootstrap.min.css"
         get "/bootstrap/js/bootstrap.min.js" $ file "website/bootstrap/js/bootstrap.min.js"
         get "/res/background.jpg" $ file "website/res/background.jpg"
         get "/" $ file "website/index.html"

         get "/search" $
             
             html . renderText $
             -- doctype_ html_ $ do
                  html_ [lang_ "en"] $ do 
                       head_ $ do
                         meta_ [charset_ "utf-8"]
                         meta_ [httpEquiv_ "X-UA-Compatible", content_ "IE-edge"]
                         title_ "Game Finder"
                         link_ [href_ "bootstrap/css/bootstrap.min.css", rel_ "stylesheet", type_ "text/css"]
                         link_ [href_ "css/index.css", rel_ "stylesheet", type_ "text/css"]
                       body_ $ do
                         -- h1_ "Submitted data"
                         nav_ [class_ "navbar navbar-inverse"] $ do
                                         div_ [class_ "container-fluid"] $ do
                                                                   div_ [class_ "navbar-header"] $ do
                                                                             a_ [class_ "navbar-brand", href_ "#"] "Cheap Game Finder"
                         div_ [class_ "background"] ""
                         toHtmlRaw ("<script src=\"https://ajax.googleapis.com/ajax/libs/jquery/1.11.2/jquery.min.js\"></script>" :: Text)
                         toHtmlRaw ("<script src=\"bootstrap/js/bootstrap.min.js\"></script>" :: Text)

         
                     
