{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Lucid
import Lucid.Base
import Data.Monoid
import Control.Applicative
import Web.Scotty
import qualified Walmart as W 

main :: IO ()
main = scotty 3000 $ do
         get "/css/index.css" $ file "css/index.css"
         get "/bootstrap/css/bootstrap.min.css" $ file "bootstrap/css/bootstrap.min.css"
         get "/bootstrap/js/bootstrap.min.js" $ file "bootstrap/js/bootstrap.min.js"
         get "/res/background.jpg" $ file "res/background.jpg"
         get "/" $ file "./index.html"

         get "/search" $
             
             html . renderText $
                  html_ $ do 
                       body_ $ do
                            h1_ "Submitted data"

         
                     
