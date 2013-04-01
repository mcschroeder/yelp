{-# LANGUAGE OverloadedStrings #-}

module Web.Yelp.Types 
	( Credentials(..)
	, SearchResult(..)
	, Business(..)
	) where

import Control.Applicative
import Control.Monad (mzero)
import qualified Data.Aeson as A
import Data.Aeson ((.:), (.:?))
import Data.ByteString (ByteString)
import Data.Text (Text)

-- | Yelp API access credentials.
data Credentials = 
    Credentials { consumerKey    :: ByteString
                , consumerSecret :: ByteString
                , token          :: ByteString
                , tokenSecret    :: ByteString
                } deriving (Show)

data SearchResult =
	SearchResult { searchResultTotal      :: Integer
				 , searchResultBusinesses :: [Business]
				 } deriving (Show)

instance A.FromJSON SearchResult where
	parseJSON (A.Object v) =
		SearchResult <$> v .: "total"
					 <*> v .: "businesses"

data Business = 
	Business { businessId        :: Text 
			 , businessIsClaimed :: Bool
			 , businessIsCloses  :: Bool
			 , businessName 	 :: Text
			 } deriving (Show)

instance A.FromJSON Business where
	parseJSON (A.Object v) =
		Business <$> v .: "id"
				 <*> v .: "is_claimed"
				 <*> v .: "is_closed"
				 <*> v .: "name"
	parseJSON _ = mzero
