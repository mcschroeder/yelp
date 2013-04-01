module Web.Yelp.Search
	(search
	) where

import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (MonadResource)
import Data.ByteString (ByteString)

import Web.Yelp.Base
import Web.Yelp.Monad
import Web.Yelp.Types

search :: (MonadBaseControl IO m, MonadResource m) => 
		  ByteString 
	   -> ByteString 
	   -> YelpT m ByteString
search term location = do
    let req = yreq "/v2/search" [("term",term), ("location",location)]
    res <- yhttp req
    body <- asBS res
    return body
