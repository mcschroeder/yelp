{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Yelp.Business
    ( getBusiness
    , BusinessId
    , Business(..)
    , Location(..)
    ) where

import Control.Applicative
import Control.Monad (mzero)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (MonadResource)
import Data.Aeson ((.:),(.:?))
import Data.Text (Text)

import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Network.HTTP.Types as HT
import qualified Network.HTTP.Types.QueryLike as HT

import Web.Yelp.Base
import Web.Yelp.Monad
import Web.Yelp.Types

-- | Lookup business information by ID.
getBusiness :: (MonadResource m, MonadBaseControl IO m) =>
               BusinessId       -- ^ Yelp business ID
            -> Maybe Locale
            -> YelpT m Business
getBusiness bid locale = 
    getObject ("/v2/business/" `T.append` bid) (HT.toQuery locale)


-- | ID of a business on Yelp.
type BusinessId = Text

-- | A business registered on Yelp.
-- Note that at this time there is no support for 
-- deals, gift certificates or reviews
data Business = Business 
    { -- | Yelp ID for this business
      businessId :: BusinessId

      -- | Whether business has been claimed by a business owner
    , businessIsClaimed :: Bool

      -- | Whether business has been (permanently) closed
    , businessIsClosed :: Bool

      -- | Name of this business
    , businessName :: Text

      -- | URL of photo for this business
    , businessImageUrl :: Text

      -- | URL for business page on Yelp
    , businessUrl :: Text

      -- | URL for mobile business page on Yelp
    , businessMobileUrl :: Text

      -- | Phone number for this business with international dialing code 
      -- (e.g. +442079460000)
    , businessPhone :: Maybe Text

      -- | Phone number for this business formatted for display
    , businessDisplayPhone :: Maybe Text

      -- | Number of reviews for this business
    , businessReviewCount :: Integer

      -- | Provides a list of @(category name, alias)@ pairs 
      -- that this business is associated with.
      -- For example, @[["Local Flavor", "localflavor"], 
      -- ["Active Life", "active"], ["Mass Media", "massmedia"]]@
      -- This alias is provided so you can use it with 'SearchFilter'.
    , businessCategories :: [(Text,Text)]

      -- | Distance that business is from search location in meters, 
      -- if a latitude/longitude is specified.
    , businessDistance :: Maybe Double

      -- | Rating for this business (value ranges from 1, 1.5, ... 4.5, 5)
    , businessRating :: Double

      -- | URL to star rating image for this business (size = 84x17)
    , businessRatingImageUrl :: Text

      -- | URL to small version of rating image for this business 
      -- (size = 50x10)
    , businessRatingImageUrlSmall :: Text

      -- | URL to large version of rating image for this business 
      -- (size = 166x30)
    , businessRatingImageUrlLarge :: Text

      -- | Snippet text associated with this business
    , businessSnippetText :: Maybe Text

      -- | URL of snippet image associated with this business
    , businessSnippetImageUrl :: Maybe Text

      -- | Location data for this business
    , businessLocation :: Location
    } deriving (Show)

instance A.FromJSON Business where
    parseJSON (A.Object v) =
        Business <$> v .:  "id"
                 <*> v .:  "is_claimed"
                 <*> v .:  "is_closed"
                 <*> v .:  "name"
                 <*> v .:  "image_url"
                 <*> v .:  "url"
                 <*> v .:  "mobile_url"
                 <*> v .:? "phone"
                 <*> v .:? "display_phone"
                 <*> v .:  "review_count"
                 <*> v .:  "categories"
                 <*> v .:? "distance"
                 <*> v .:  "rating"
                 <*> v .:  "rating_img_url"
                 <*> v .:  "rating_img_url_small"
                 <*> v .:  "rating_img_url_large"
                 <*> v .:? "snippet_text"
                 <*> v .:? "snippet_image_url"
                 <*> v .:  "location"
    parseJSON _ = mzero


-- | Location data for a business
data Location = Location
    { locationCoordinates    :: Coordinates
    , locationAddress        :: [Text]
    , locationCity           :: Text
    , locationStateCode      :: Text -- ^ ISO 3166-2 state code
    , locationPostalCode     :: Maybe Text      
    , locationCountryCode    :: Text -- ^ ISO 3166-1 country code
    , locationCrossStreets   :: Maybe Text
    , locationNeighbourhoods :: [Text]

      -- | Address for the business formatted for display.
      -- Includes all address fields, cross streets and city, state code etc.
    , locationDisplayAddress :: [Text]

      -- | Contains a value that corresponds to the accuracy with which 
      -- the latitude / longitude was determined in the geocoder. 
      -- These correspond to Google's GGeoAddressAccuracy field.
    , locationGeoAccuracy :: Integer
    } deriving (Show)

instance A.FromJSON Location where
    parseJSON (A.Object v) =
        Location <$> v .:  "coordinate"
                 <*> v .:  "address"                 
                 <*> v .:  "city"
                 <*> v .:  "state_code"
                 <*> v .:? "postal_code"
                 <*> v .:  "country_code"
                 <*> v .:? "cross_streets"
                 <*> v .:  "neighborhoods"
                 <*> v .:  "display_address"
                 <*> v .:  "geo_accuracy"
