{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Web.Yelp.Base
	( getObject
    , yreq
	, yhttp
    , asJson
	, asBS
    , YelpException(..)
	) where

import Control.Applicative
import Control.Monad (mzero)
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson ((.:),(.:?))
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Typeable (Typeable)

import qualified Control.Exception.Lifted as E
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.Conduit as C
import qualified Data.Conduit.Attoparsec as C
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Network.HTTP.Conduit as H
import qualified Network.HTTP.Types as HT
import qualified Web.Authenticate.OAuth as OA

import Web.Yelp.Monad
import Web.Yelp.Types

-- | Make a raw @GET@ request to Yelp's API.
getObject :: (C.MonadResource m, C.MonadBaseControl IO m, A.FromJSON a) =>
             Text   -- ^ Path (should begin with a slash @\/@)
          -> HT.Query
          -> YelpT m a
getObject path query =
    runResourceInY $ asJson =<< yhttp =<< yreq path query

-- | A plain 'H.Request' to a Yelp API.
yreq :: Monad m => Text -> HT.Query -> YelpT m (H.Request n)
yreq path query = return
    H.def { H.host = "api.yelp.com"
          , H.path = encodeUtf8 path
          , H.queryString = HT.renderQuery False query
          , H.responseTimeout = Just 120000000 -- 2 minutes
          }

-- | Same as 'H.http', but takes care of all the boilerplate
-- (such as signing the request and error handling).
yhttp :: (MonadBaseControl IO m, C.MonadResource m) => 
         H.Request m 
      -> YelpT m (H.Response (C.ResumableSource m ByteString))
yhttp req = do
    let req' = req { H.checkStatus = \_ _ _ -> Nothing }
    manager <- getManager
    (oauth, cred) <- getOAuth
    lift $ do
        signedReq <- OA.signOAuth oauth cred req'
        response <- H.http signedReq manager
        let status  = H.responseStatus    response
            headers = H.responseHeaders   response
            cookies = H.responseCookieJar response
        if isOkay status
            then return response
            else do
                let statusexc = H.StatusCodeException status headers cookies
                val <- E.try $ asJsonHelper response
                case val :: Either E.SomeException YelpException of
                    Right yelpexc -> E.throw   yelpexc
                    Left _        -> E.throwIO statusexc

-- | @True@ if the the 'Status' is ok (i.e. @2XX@).
isOkay :: HT.Status -> Bool
isOkay status =
    let sc = HT.statusCode status
    in 200 <= sc && sc < 300

-- | Converts a plain 'H.Response' coming from 'H.http' into a JSON value.
asJson :: (MonadTrans t, C.MonadThrow m, A.FromJSON a) =>
          H.Response (C.ResumableSource m ByteString)
       -> t m a
asJson = lift . asJsonHelper

asJsonHelper :: (C.MonadThrow m, A.FromJSON a) =>
                H.Response (C.ResumableSource m ByteString)
             -> m a
asJsonHelper response = do
  val <- H.responseBody response C.$$+- C.sinkParser A.json'
  case A.fromJSON val of
    A.Success r -> return r
    A.Error str -> 
        E.throw $ YelpLibraryException $ T.concat
            [ "Yelp.Base.asJson: could not parse Yelp's response "
            , "(", T.pack str, ")" ]

-- | Converts a plain 'H.Response' into a 'ByteString'.
asBS :: Monad m =>
        H.Response (C.ResumableSource m ByteString)
     -> YelpT m ByteString
asBS response = lift $ H.responseBody response C.$$+- fmap B.concat CL.consume


-- | An exception that may be thrown by functions in this package.
data YelpException 
      -- | An exception coming from Yelp.
      -- See <http://www.yelp.com/developers/documentation/v2/errors>.
    = YelpException { exceptionText  :: Text
                    , exceptionId    :: Text
                    , exceptionField :: Maybe Text
                    }
      -- | An exception coming from the @yelp@ package's code.
    | YelpLibraryException { exceptionText :: Text }
      deriving (Eq, Ord, Show, Read, Typeable)

instance A.FromJSON YelpException where
    parseJSON (A.Object v) = do
        v' <- v .: "error"
        YelpException <$> v' .:  "text"
                      <*> v' .:  "id"
                      <*> v' .:? "field"
    parseJSON _ = mzero

instance E.Exception YelpException
