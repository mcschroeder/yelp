{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Yelp.Business
    ( getBusiness
    , BusinessId
    , Business(..)
    , Location(..)
    , Deal(..)
    , DealOption(..)
    , GiftCertificate(..)
    , Price(..)
    , Review(..)
    , Rating(..)
    , User(..)
    ) where

import Control.Applicative
import Control.Monad (mzero)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (MonadResource)
import Data.Aeson ((.:),(.:?),(.!=))
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

      -- | Rating for this business
    , businessRating :: Rating

      -- | Snippet text associated with this business
    , businessSnippetText :: Maybe Text

      -- | URL of snippet image associated with this business
    , businessSnippetImageUrl :: Maybe Text

      -- | Location data for this business
    , businessLocation :: Location

      -- Deal info for this business
    , businessDeals :: [Deal]

      -- Gift certificate info for this business
    , businessGiftCertificates :: [GiftCertificate]

    , businessReviews :: [Review]
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
                 <*> (Rating <$> v .: "rating"
                             <*> v .: "rating_img_url"
                             <*> v .: "rating_img_url_small"
                             <*> v .: "rating_img_url_large")
                 <*> v .:? "snippet_text"
                 <*> v .:? "snippet_image_url"
                 <*> v .:  "location"
                 <*> v .:? "deals" .!= []
                 <*> v .:? "gift_certificates" .!= []
                 <*> v .:? "reviews" .!= []
    parseJSON _ = mzero


-- | Location data for a business
data Location = Location
    { locationCoordinates    :: Coordinates
    , locationAddress        :: [Text]
    , locationCity           :: Text
    , locationStateCode      :: Text  -- ^ ISO 3166-2 state code
    , locationPostalCode     :: Maybe Text      
    , locationCountryCode    :: Text  -- ^ ISO 3166-1 country code
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
                 <*> v .:? "neighborhoods" .!= []
                 <*> v .:  "display_address"
                 <*> v .:  "geo_accuracy"


-- | Deal data for a business
data Deal = Deal
    { dealId                     :: Text
    , dealTitle                  :: Text
    , dealUrl                    :: Text
    , dealImageUrl               :: Text
    , dealCurrencyCode           :: Text  -- ^ ISO 4217 Currency Code
    , dealTimeStart              :: Integer  -- ^ Unix timestamp
    , dealTimeEnd                :: Maybe Integer
    , dealIsPopular              :: Bool
    , dealWhatYouGet             :: Text  -- ^ Additional details
    , dealImportantRestrictions  :: Maybe Text
    , dealAdditionalRestrictions :: Text
    , dealOptions                :: [DealOption]
    } deriving (Show)

instance A.FromJSON Deal where
    parseJSON (A.Object v) =
        Deal <$> v .:  "id"
             <*> v .:  "title"
             <*> v .:  "url"
             <*> v .:  "image_url"
             <*> v .:  "currency_code"
             <*> v .:  "time_start"
             <*> v .:? "time_end"
             <*> v .:? "is_popular" .!= False
             <*> v .:  "what_you_get"
             <*> v .:? "important_restrictions"
             <*> v .:  "additional_restrictions"
             <*> v .:  "options"


-- | Deal option
data DealOption = DealOption
    { dealOptionTitle         :: Text
    , dealOptionPurchaseUrl   :: Text
    , dealOptionPrice         :: Price  -- ^ Price after discount
    , dealOptionOriginalPrice :: Price  -- ^ Price before discount

      -- | The remaining deal options available for purchase
      -- ('Nothing' if the deal is unlimited)
    , dealOptionRemainingQuantity :: Maybe Integer
    } deriving (Show)

instance A.FromJSON DealOption where
    parseJSON (A.Object v) =
        DealOption <$> v .:  "title"
                   <*> v .:  "purchase_url"
                   <*> (Price <$> v .: "price"
                              <*> v .: "formatted_price")
                   <*> (Price <$> v .: "original_price"
                              <*> v .: "formatted_original_price")
                   <*> v .:? "remaining_count"


data GiftCertificate = GiftCertificate
    { giftCertificateId :: Text
    , giftCertificateUrl :: Text
    , giftCertificateImageUrl :: Text
    , giftCertificateCurrencyCode :: Text  -- ^ ISO 4217 Currency Code

      -- | Whether unused balances are returned as cash or store credit
    , giftCertificateUnusedBalances :: Text
    , giftCertificateOptions :: [Price]
    } deriving (Show)

instance A.FromJSON GiftCertificate where
    parseJSON (A.Object v) =
        GiftCertificate <$> v .: "id"
                        <*> v .: "url"
                        <*> v .: "image_url"
                        <*> v .: "currency_code"
                        <*> v .: "unused_balances"
                        <*> v .: "options"


data Price = Price
    { price          :: Double  -- ^ Price in cents
    , formattedPrice :: Text    -- ^ Formatted price, e.g. @$6@
    } deriving (Show)

instance A.FromJSON Price where
    parseJSON (A.Object v) =
        Price <$> v .: "price"
              <*> v .: "formatted_price"


-- | Business review
data Review = Review
    { reviewId          :: Text
    , reviewRating      :: Rating
    , reviewExcerpt     :: Text
    , reviewTimeCreated :: Integer  -- ^ Unix timestamp
    , reviewUser        :: User  -- ^ User who wrote the review
    } deriving (Show)

instance A.FromJSON Review where
    parseJSON (A.Object v) =
        Review <$> v .: "id"
               <*> (Rating <$> v .: "rating"
                           <*> v .: "rating_image_url"
                           <*> v .: "rating_image_small_url"
                           <*> v .: "rating_image_large_url")
               <*> v .: "excerpt"
               <*> v .: "time_created"
               <*> v .: "user"


-- | Rating for a business
data Rating = Rating
    { -- | Rating from 1-5
      rating :: Double

      -- | URL to star rating image (size = 84x17)
    , ratingImageUrl :: Text
      
      -- | URL to small version of rating image (size = 50x10)
    , ratingImageUrlSmall :: Text
      
      -- | URL to large version of rating image (size = 166x30)
    , ratingImageUrlLarge :: Text
    } deriving (Show)


-- | Yelp user
data User = User
    { userId       :: Text
    , userImageUrl :: Text
    , userName     :: Text
    } deriving (Show)

instance A.FromJSON User where
    parseJSON (A.Object v) =
        User <$> v .: "id"
             <*> v .: "image_url"
             <*> v .: "name"

