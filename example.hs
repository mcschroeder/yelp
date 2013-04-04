{-# LANGUAGE OverloadedStrings #-}

import qualified Web.Yelp as Y

import Network.HTTP.Conduit (withManager)
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = do
    credentials <- read `fmap` readFile "credentials"
    --let credentials = Y.Credentials "cKey" "cSecret" "token" "tokenSecret"
    withManager $ \manager ->
        Y.runYelpT credentials manager $ do
            results <- Y.simpleSearch "vienna" (Just "schnitzel")
            liftIO $ print results
