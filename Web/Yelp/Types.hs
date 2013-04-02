{-# LANGUAGE OverloadedStrings #-}

module Web.Yelp.Types 
	( Credentials(..)
	, SearchResult(..)
	, Business(..)

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
    { searchResultTotal      :: Integer
    , searchResultBusinesses :: [Business]
	} deriving (Show)

instance A.FromJSON SearchResult where
	parseJSON (A.Object v) =
		SearchResult <$> v .: "total"
					 <*> v .: "businesses"

data Business = Business 
    { businessId        :: Text 
    , businessIsClaimed :: Bool
    , businessIsClosed  :: Bool
    , businessName 	 :: Text
    } deriving (Show)

instance A.FromJSON Business where
	parseJSON (A.Object v) =
		Business <$> v .: "id"
				 <*> v .: "is_claimed"
				 <*> v .: "is_closed"
				 <*> v .: "name"
	parseJSON _ = mzero

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
