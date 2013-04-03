{-# LANGUAGE OverloadedStrings #-}

module Web.Yelp.Types 
    ( Credentials(..)
    , SearchResult(..)
    , Business(..)
    , Region(..)
    , Location(..)
    , CoordinateSpan(..)
    , Coordinates(..)
    , coordsAsBS

      -- * API parameters
    , Locale(..)

    , (.=)
    ) where

import Control.Applicative
import Control.Monad (mzero)
import qualified Data.Aeson as A
import Data.Aeson ((.:), (.:?))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import Data.Text (Text)
import qualified Data.Text as T
import qualified Network.HTTP.Types as HT
import qualified Network.HTTP.Types.QueryLike as HT


-- | Yelp API access credentials.
data Credentials = Credentials 
    { consumerKey    :: ByteString
    , consumerSecret :: ByteString
    , token          :: ByteString
    , tokenSecret    :: ByteString
    } deriving (Show)


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

-- | A business registered on Yelp.
-- Note that at this time there is no support for 
-- deals, gift certificates or reviews
data Business = Business 
    { -- | Yelp ID for this business
      businessId :: Text

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


-- | Suggested bounds in a map to display results in
data Region = Region
    { regionSpan   :: CoordinateSpan -- ^ Span of suggested map bounds
    , regionCenter :: Coordinates    -- ^ Center position of map bounds
    } deriving (Show)

instance A.FromJSON Region where
    parseJSON (A.Object v) =
        Region <$> v .: "span"
               <*> v .: "center"
    parseJSON _ = mzero


-- | Defines the area spanned by a map region
data CoordinateSpan = CoordinateSpan
    { latitudeDelta  :: Double
    , longitudeDelta :: Double
    } deriving (Show)

instance A.FromJSON CoordinateSpan where
    parseJSON (A.Object v) =
        CoordinateSpan <$> v .: "latitude_delta"
                       <*> v .: "longitude_delta"
    parseJSON _ = mzero


-- | Geographical coordinates
data Coordinates = Coordinates 
    { latitude  :: Double
    , longitude :: Double
    } deriving (Show)

instance A.FromJSON Coordinates where
    parseJSON (A.Object v) =
        Coordinates <$> v .: "latitude"
                    <*> v .: "longitude"
    parseJSON _ = mzero

-- | Returns a 'ByteString' of the form @\"latitude,longitude\"@.
coordsAsBS :: Coordinates -> ByteString
coordsAsBS (Coordinates lat lon) = 
    B.intercalate "," $ map (BC.pack . show) [lat,lon]


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


-- | Results will be localized in the region format and language if supported.
data Locale = Locale 
    { -- | ISO 3166-1 alpha-2 country code. 
      -- Default country to use when parsing the location field. 
      -- United States = US, Canada = CA, United Kingdom = GB (not UK).
      countryCode :: Text

      -- | ISO 639 language code.
      -- Reviews written in the specified language will be shown.
    , language :: Text

      -- | Whether to filter business reviews by the specified language
    , filterByLanguage :: Bool
    } deriving (Show)

instance HT.QueryLike Locale where
    toQuery (Locale cc lang lang_filter) =
        ["cc" .= cc, "lang" .= lang, "lang_filter" .= lang_filter]

-- | Constructs a 'HT.QueryItem' from a key and a 'HT.QueryValueLike' value.
(.=) :: (HT.QueryValueLike v) => ByteString -> v -> HT.QueryItem
k .= v = (k, HT.toQueryValue v)

instance HT.QueryValueLike Bool where
    toQueryValue True  = Just "true"
    toQueryValue False = Just "false"

instance HT.QueryValueLike Integer where
    toQueryValue = Just . BC.pack . show

instance (HT.QueryLike a) => HT.QueryLike (Maybe a) where
    toQuery (Just a) = HT.toQuery a
    toQuery Nothing  = []

instance (HT.QueryKeyLike k, HT.QueryValueLike v) => HT.QueryLike (k, v) where
    toQuery (k,v) = [(HT.toQueryKey k, HT.toQueryValue v)]
