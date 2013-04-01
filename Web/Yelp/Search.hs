{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Yelp.Search
    ( search
    , search'
    
    , Paging(..)
    , SortOption(..)
    , SearchFilter(..)
    , Coordinates(..)
    , LocationQuery(..)
    , BoundingBox(..)
    , SearchCoordinates(..)
    , Neighbourhood(..)
    ) where

import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (MonadResource)
import Data.ByteString (ByteString)
import qualified Network.HTTP.Types as HT
import qualified Network.HTTP.Types.QueryLike as HT

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L

import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

import Web.Yelp.Base
import Web.Yelp.Monad
import Web.Yelp.Types

search :: (MonadResource m, MonadBaseControl IO m) => 
           HT.Query 
        -> YelpT m SearchResult
search query = 
    runResourceInY $ asJson =<< yhttp =<< yreq "/v2/search" query


-- | Search for local businesses.
search' :: (MonadResource m, MonadBaseControl IO m) =>
          LocationQuery 
       -> Maybe Text        -- ^ Optional search term
       -> Maybe Paging
       -> SortOption
       -> SearchFilter
       -> Maybe Locale 
       -> YelpT m SearchResult
search' location term paging options sfilter locale =
    search $ HT.toQuery location 
          ++ HT.toQuery (fmap ("term" .=) term)
          ++ HT.toQuery paging
          ++ HT.toQuery options
          ++ HT.toQuery sfilter
          ++ HT.toQuery locale

data Paging = Paging { pagingLimit :: Integer
                     , pagingOffset :: Integer
                     } deriving (Show)

instance HT.QueryLike Paging where
    toQuery (Paging limit offset) = ["limit" .= limit, "offset" .= offset]

data SortOption = SortByMatch | SortByDistance | SortByRating

instance HT.QueryLike SortOption where
    toQuery SortByMatch    = [("sort", Just "0")]
    toQuery SortByDistance = [("sort", Just "1")]
    toQuery SortByRating   = [("sort", Just "2")]

data SearchFilter = 
    SearchFilter { filterCategories :: [Text]
                 , filterRadius     :: Maybe Integer
                 , filterDeals      :: Bool
                 } deriving (Show)

instance HT.QueryLike SearchFilter where
    toQuery (SearchFilter cs radius deals) =
        catMaybes [cs',radius',deals']
        where cs'     = if null cs then Nothing
                        else Just ("category_filter" .= T.intercalate "," cs)
              radius' = fmap ("radius_filter" .=) radius
              deals'  = if deals then Just ("deals_filter" .= True)
                        else Nothing

data Coordinates = 
    Coordinates { latitude  :: Double
                , longitude :: Double
                } deriving (Show)

-- | Returns a 'ByteString' of the form @\"latitude,longitude\"@.
coordsAsBS :: Coordinates -> ByteString
coordsAsBS (Coordinates lat lon) = 
    B.intercalate "," $ map (BC.pack . show) [lat,lon]

data LocationQuery = BoundingBoxQuery BoundingBox
                   | CoordinateQuery SearchCoordinates
                   | NeighbourhoodQuery Neighbourhood

instance HT.QueryLike LocationQuery where
    toQuery (BoundingBoxQuery q)   = HT.toQuery q
    toQuery (CoordinateQuery q)    = HT.toQuery q
    toQuery (NeighbourhoodQuery q) = HT.toQuery q

data BoundingBox = 
    BoundingBox { boundingBoxSWCoordinates :: Coordinates
                , boundingBoxNECoordinates :: Coordinates
                } deriving (Show)

instance HT.QueryLike BoundingBox where
    toQuery (BoundingBox sw ne) =
        ["bounds" .= B.concat [coordsAsBS sw, "|", coordsAsBS ne]]

data SearchCoordinates = 
    SearchCoordinates { searchCoordinates           :: Coordinates
                      , searchCoordAccuracy         :: Maybe Double
                      , searchCoordAltitude         :: Maybe Double
                      , searchCoordAltitudeAccuracy :: Maybe Double
                      } deriving (Show)

instance HT.QueryLike SearchCoordinates where
    toQuery (SearchCoordinates (Coordinates lat lon) acc alt altacc) = 
        [("ll", Just params)] 
        where params = B.intercalate "," $ map (BC.pack . show) $ 
                       [lat,lon] ++ catMaybes [acc,alt,altacc]

data Neighbourhood =
    Neighbourhood { neighbourhoodLocation    :: Text
                  , neighbourhoodCoordinates :: Maybe Coordinates
                  } deriving (Show)

instance HT.QueryLike Neighbourhood where
    toQuery (Neighbourhood location coords) =
        ("location" .= location):cll
        where cll = case coords of
                        Nothing -> []
                        Just cs -> ["cll" .= coordsAsBS cs]
