{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Yelp.Search
    ( search
    , simpleSearch
    , Paging(..)
    , SortOption(..)
    , SearchFilter(..)
    , LocationQuery(..)
    , BoundingBox(..)
    , SearchCoordinates(..)
    , Neighbourhood(..)    
    , SearchResult(..)
    , Region(..)
    , CoordinateSpan(..)
    ) where

import Control.Applicative
import Control.Monad (mzero)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (MonadResource)
import Data.Aeson ((.:),(.:?))
import Data.ByteString (ByteString)
import Data.Maybe
import Data.Text (Text)

import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Network.HTTP.Types as HT
import qualified Network.HTTP.Types.QueryLike as HT

import Web.Yelp.Base
import Web.Yelp.Business
import Web.Yelp.Monad
import Web.Yelp.Types


-- | Search for local businesses. This is a simplified version of 'search' 
-- that only takes a natural language location and search term.
simpleSearch :: (MonadResource m, MonadBaseControl IO m) =>
                Text        -- ^ Location (like in 'neighbourhoodLocation')
             -> Maybe Text  -- ^ Optional search term
             -> YelpT m SearchResult
simpleSearch location term =
    search (NeighbourhoodQuery (Neighbourhood location Nothing))
           term Nothing SortByMatch Nothing Nothing


-- | Search for local businesses.
search :: (MonadResource m, MonadBaseControl IO m) =>
          LocationQuery 
       -> Maybe Text        -- ^ Optional search term
       -> Maybe Paging
       -> SortOption
       -> Maybe SearchFilter
       -> Maybe Locale 
       -> YelpT m SearchResult
search location term paging options sfilter locale =
    getObject "/v2/search" $ HT.toQuery location 
                          ++ HT.toQuery (fmap ("term" .=) term)
                          ++ HT.toQuery paging
                          ++ HT.toQuery options
                          ++ HT.toQuery sfilter
                          ++ HT.toQuery locale


-- | You can use this data structure to limit the amount of results
-- returned by a search and to offset into the total result list
-- (i.e. you can request results "page-by-page".)
--
-- The maximum number of results you can retrieve at once is 20.
-- 
-- Using the offset you can request up to and including the 1000th result, 
-- except when sorting by distance or rating, which limits you to 40 results.
data Paging = Paging 
    { pagingLimit :: Integer   -- ^ Number of results to return
    , pagingOffset :: Integer  -- ^ Offset the list of results by this amount
    } deriving (Show)

instance HT.QueryLike Paging where
    toQuery (Paging limit offset) = ["limit" .= limit, "offset" .= offset]


-- | Sort mode used when searching.
-- 
-- Sort by distance is only supported for a location or geographic search. 
--
-- The rating sort is not strictly sorted by the rating value, 
-- but by an adjusted rating value that takes into account the number 
-- of ratings, similar to a bayesian average. This is so a business with 
-- 1 rating of 5 stars doesn't immediately jump to the top.
data SortOption = SortByMatch | SortByDistance | SortByRating

instance HT.QueryLike SortOption where
    toQuery SortByMatch    = [("sort", Just "0")]
    toQuery SortByDistance = [("sort", Just "1")]
    toQuery SortByRating   = [("sort", Just "2")]


-- | Additional constraints to filter the search results with.
data SearchFilter = SearchFilter 
    { -- | See the list of supported categories: 
      -- <http://www.yelp.com/developers/documentation/category_list>
     filterCategories :: [Text]
    
      -- | Search radius in meters. The max value is 40000 meters (25 miles).
    , filterRadius :: Maybe Integer
    
      -- | Whether to exclusively search for businesses with deals
    , filterDeals :: Bool
    } deriving (Show)

instance HT.QueryLike SearchFilter where
    toQuery (SearchFilter cs radius deals) =
        catMaybes [cs',radius',deals']
        where cs'     = if null cs then Nothing
                        else Just ("category_filter" .= T.intercalate "," cs)
              radius' = fmap ("radius_filter" .=) radius
              deals'  = if deals then Just ("deals_filter" .= True)
                        else Nothing


-- | Method of specifying location in a search.
data LocationQuery = BoundingBoxQuery BoundingBox
                   | CoordinateQuery SearchCoordinates
                   | NeighbourhoodQuery Neighbourhood

instance HT.QueryLike LocationQuery where
    toQuery (BoundingBoxQuery q)   = HT.toQuery q
    toQuery (CoordinateQuery q)    = HT.toQuery q
    toQuery (NeighbourhoodQuery q) = HT.toQuery q


-- | A location that is specified by a geographical bounding box, 
-- defined by southwest and northeast coordinates.
data BoundingBox = BoundingBox 
    { boundingBoxSWCoordinates :: Coordinates  -- ^ Southwest corner
    , boundingBoxNECoordinates :: Coordinates  -- ^ Northeast corner
    } deriving (Show)

instance HT.QueryLike BoundingBox where
    toQuery (BoundingBox sw ne) =
        ["bounds" .= B.concat [coordsAsBS sw, "|", coordsAsBS ne]]


-- | A location that is specified by geographic coordinates.
data SearchCoordinates = SearchCoordinates 
    { searchCoordinates           :: Coordinates   -- ^ Geo-point to search near
    , searchCoordAccuracy         :: Maybe Double  -- ^ Accuracy of coordinates
    , searchCoordAltitude         :: Maybe Double  -- ^ Altitude
    , searchCoordAltitudeAccuracy :: Maybe Double  -- ^ Accuracy of altitude
    } deriving (Show)

instance HT.QueryLike SearchCoordinates where
    toQuery (SearchCoordinates (Coordinates lat lon) acc alt altacc) = 
        [("ll", Just params)] 
        where params = B.intercalate "," $ map (BC.pack . show) $ 
                       [lat,lon] ++ catMaybes [acc,alt,altacc]


-- | A location specified by a particular neighbourhood, address or city.
data Neighbourhood = Neighbourhood 
    { -- | Combination of \"address, neighbourhood, city, state or zip, 
      -- optional country\"
      neighbourhoodLocation    :: Text

      -- | Optional hint to the geocoder to disambiguate the location text
    , neighbourhoodCoordinates :: Maybe Coordinates
    } deriving (Show)

instance HT.QueryLike Neighbourhood where
    toQuery (Neighbourhood location coords) =
        ("location" .= location):cll
        where cll = case coords of
                        Nothing -> []
                        Just cs -> ["cll" .= coordsAsBS cs]


-- | The results of a search request.
data SearchResult = SearchResult 
    { searchResultRegion     :: Region
    , searchResultTotal      :: Integer  -- ^ Total number of business results
    , searchResultBusinesses :: [Business]
    } deriving (Show)

instance A.FromJSON SearchResult where
    parseJSON (A.Object v) =
        SearchResult <$> v .: "region"
                     <*> v .: "total"
                     <*> v .: "businesses"
    parseJSON _ = mzero


-- | Suggested bounds in a map to display results in.
data Region = Region
    { regionSpan   :: CoordinateSpan -- ^ Span of suggested map bounds
    , regionCenter :: Coordinates    -- ^ Center position of map bounds
    } deriving (Show)

instance A.FromJSON Region where
    parseJSON (A.Object v) =
        Region <$> v .: "span"
               <*> v .: "center"
    parseJSON _ = mzero


-- | Defines the area spanned by a map region.
data CoordinateSpan = CoordinateSpan
    { latitudeDelta  :: Double
    , longitudeDelta :: Double
    } deriving (Show)

instance A.FromJSON CoordinateSpan where
    parseJSON (A.Object v) =
        CoordinateSpan <$> v .: "latitude_delta"
                       <*> v .: "longitude_delta"
    parseJSON _ = mzero
