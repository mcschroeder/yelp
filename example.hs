{-# LANGUAGE OverloadedStrings #-}

import Web.Yelp

import Network.HTTP.Conduit (withManager)
import Control.Monad.IO.Class (liftIO)

creds = Credentials { consumerKey    = "your"
                    , consumerSecret = "secret"
                    , token          = "api"
                    , tokenSecret    = "credentials" 
                    }

main :: IO ()
main = withManager $ \manager -> do
    runYelpT creds manager $ do
        --res <- search [("term",Just "food"),("location",Just "San Francisco")]
        res <- search' (NeighbourhoodQuery (Neighbourhood "Wien" Nothing))
                       Nothing
                       (Just (Paging { pagingLimit = 20, pagingOffset = 0 }))
                       SortByMatch
                       SearchFilter { filterCategories = ["bars","french"]
                                    , filterRadius     = Nothing
                                    , filterDeals      = False
                                    }
                       (Just (Locale { countryCode = "AT", language = "en" 
                                     , filterByLanguage = False
                                     }))
        liftIO $ print res
