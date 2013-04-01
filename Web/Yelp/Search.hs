{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Yelp.Search
	( search
	) where

import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (MonadResource)
import Data.ByteString (ByteString)
import Network.HTTP.Types (SimpleQuery)

import Web.Yelp.Base
import Web.Yelp.Monad
import Web.Yelp.Types

search :: (MonadResource m, MonadBaseControl IO m) => 
		   SimpleQuery 
		-> YelpT m SearchResult
search query = 
	runResourceInY $ asJson =<< yhttp =<< yreq "/v2/search" query