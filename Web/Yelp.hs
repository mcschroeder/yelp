{-# LANGUAGE OverloadedStrings #-}

module Web.Yelp 
    ( -- * @YelpT@ monad transformer
      YelpT
    , runYelpT

      -- * Authentication
    , Credentials(..)

      -- * Yelp's Search API
      -- ** Request
    , simpleSearch
    , search
    , LocationQuery(..)
    , BoundingBox(..)
    , SearchCoordinates(..)
    , Neighbourhood(..)
    , Paging(..)
    , SortOption(..)
    , SearchFilter(..)
      -- ** Response
    , SearchResult(..)
    , Region(..)
    , CoordinateSpan(..)

      -- * Yelp's Business API
      -- ** Request
    , getBusiness
    , BusinessId
      -- ** Response
    , Business(..)
    , Location(..)
    , Deal(..)
    , DealOption(..)
    , GiftCertificate(..)
    , Price(..)
    , Review(..)
    , Rating(..)
    , User(..)

      -- * Common Types
    , Locale(..)
    , Coordinates(..)

      -- * Raw API access
    , getObject

      -- * Exceptions
    , YelpException(..)
    ) where

import Web.Yelp.Base
import Web.Yelp.Business
import Web.Yelp.Monad
import Web.Yelp.Search
import Web.Yelp.Types
