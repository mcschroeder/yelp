module Web.Yelp.Types 
	( Credentials(..)
	) where

import Data.ByteString (ByteString)

-- | Yelp API access credentials.
data Credentials = 
    Credentials { consumerKey    :: ByteString
                , consumerSecret :: ByteString
                , token          :: ByteString
                , tokenSecret    :: ByteString
                } deriving (Show)
