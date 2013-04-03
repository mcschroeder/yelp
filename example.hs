{-# LANGUAGE OverloadedStrings #-}

import Web.Yelp

import Network.HTTP.Conduit (withManager)
import Control.Monad.IO.Class (liftIO)

creds = Credentials { consumerKey    = "your"
                    , consumerSecret = "secret"
                    , token          = "api"
                    , tokenSecret    = "key" 
                    }

main :: IO ()
main = withManager $ \manager ->
    runYelpT creds manager $ do
        --res <- search [("term",Just "food"),("location",Just "San Francisco")]
        --res <- search (NeighbourhoodQuery (Neighbourhood "Wien" Nothing))
        --               Nothing
        --               (Just (Paging { pagingLimit = 20, pagingOffset = 0 }))
        --               SortByMatch
        --               SearchFilter { filterCategories = []
        --                            , filterRadius     = Nothing
        --                            , filterDeals      = False
        --                            }
        --               (Just (Locale { countryCode = "AT", language = "de" 
        --                             , filterByLanguage = False
        --                             }))
        res <- getBusiness "yelp-san-francisco" Nothing
        liftIO $ print res
