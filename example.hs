{-# LANGUAGE OverloadedStrings #-}

import qualified Web.Yelp as Y

import Network.HTTP.Conduit (withManager)
import Control.Monad.IO.Class (liftIO)

creds = Y.Credentials { Y.consumerKey    = "your"
                      , Y.consumerSecret = "secret"
                      , Y.token          = "api"
                      , Y.tokenSecret    = "credentials" 
                      }

main :: IO ()
main = withManager $ \manager -> do
    Y.runYelpT creds manager $ do
        c <- Y.getCreds
        liftIO $ print c
