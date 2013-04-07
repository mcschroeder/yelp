{-# LANGUAGE OverloadedStrings #-}

import qualified Web.Yelp as Y

import Network.HTTP.Conduit (withManager)
import Control.Monad.IO.Class (liftIO)
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T

main :: IO ()
main = do
    credentials <- read `fmap` readFile "credentials"
    --let credentials = Y.Credentials "cKey" "cSecret" "token" "tokenSecret"

    location <- putStr "Where are you? " >> getLine
    food <- putStr "And what are you looking for? " >> getLine
    putStrLn "Let me see what I can find..."

    withManager $ \manager ->
        Y.runYelpT credentials manager $ do
            results <- Y.search
                (Y.NeighbourhoodQuery
                    (Y.Neighbourhood (T.pack location) Nothing))
                (Just (T.pack food))
                (Just (Y.Paging { Y.pagingLimit = 5, Y.pagingOffset = 0 }))
                Y.SortByMatch
                Nothing
                Nothing

            liftIO $ case Y.searchResultTotal results of
                0 -> putStrLn "Couldn't find anything!"
                n -> do putStr ("Found " ++ show n ++ " businesses. ")
                        putStrLn "Here are the 5 best matches:"
                        mapM_ (putStrLn . prettyPrintBusiness) $
                              Y.searchResultBusinesses results


prettyPrintBusiness :: Y.Business -> String
prettyPrintBusiness biz =
    let name = T.unpack $ Y.businessName biz
        rating = show $ (Y.rating . Y.businessRating) biz
        reviewCount = show $ Y.businessReviewCount biz
        address = T.unpack . T.intercalate "\n\t" $
                  Y.locationDisplayAddress . Y.businessLocation $ biz
    in concat [ "\n", name
              , "\t\t[", rating, " stars, ", reviewCount, " reviews]"
              , "\n\t", address ]
