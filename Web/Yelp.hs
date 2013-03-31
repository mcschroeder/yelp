{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Yelp 
    ( YelpT
    , runYelpT    
    , Credentials(..)
    , getCreds
    , search
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Network.HTTP.Conduit as H
import qualified Network.HTTP.Types as HT
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Control.Monad (liftM)
import Control.Monad.Base (MonadBase(..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Resource (MonadThrow, MonadResource)
import Control.Monad.Trans.Control ( MonadTransControl(..), MonadBaseControl(..) 
                                   , ComposeSt, defaultLiftBaseWith
                                   , defaultRestoreM )
import qualified Web.Authenticate.OAuth as OA
import Control.Applicative (Applicative)
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL

-- | Yelp API access credentials.
data Credentials = 
    Credentials { consumerKey    :: ByteString
                , consumerSecret :: ByteString
                , token          :: ByteString
                , tokenSecret    :: ByteString
                } deriving (Show)

-- | @YelpT m a@ is a monad transformer containing 
-- information needed to use the Yelp API.
newtype YelpT m a = Y { unY :: ReaderT YData m a }
    deriving ( Functor, Applicative, Monad
             , MonadIO, MonadResource, MonadThrow, MonadTrans)

instance MonadBase b m => MonadBase b (YelpT m) where
    liftBase = lift . liftBase

instance MonadTransControl YelpT where
    newtype StT YelpT a = YStT { unYStT :: StT (ReaderT YData) a }
    liftWith f = Y $ liftWith (\run -> f (liftM YStT . run . unY))
    restoreT   = Y . restoreT . liftM unYStT

instance MonadBaseControl b m => MonadBaseControl b (YelpT m) where
        newtype StM (YelpT m) a = StMT { unStMT :: ComposeSt YelpT m a }
        liftBaseWith = defaultLiftBaseWith StMT
        restoreM     = defaultRestoreM   unStMT

-- | Internal data of 'YelpT'.
data YData = YData { yCreds      :: Credentials 
                   , yManager    :: H.Manager
                   , yOAuth      :: (OA.OAuth, OA.Credential)
                   }

-- | Run a computation in the 'YelpT' monad transformer 
-- with your credentials.
runYelpT :: Credentials     -- ^ Your app's credentials.
         -> H.Manager       -- ^ Connection manager (see 'H.withManager')
         -> YelpT m a
         -> m a
runYelpT creds manager (Y act) = 
    runReaderT act (YData creds manager (credsToOAuth creds))

credsToOAuth :: Credentials -> (OA.OAuth, OA.Credential)
credsToOAuth (Credentials ckey csec tok toksec) = 
    let oauth = OA.newOAuth { OA.oauthConsumerKey    = ckey
                            , OA.oauthConsumerSecret = csec
                            }
        oauthcreds = OA.newCredential tok toksec
    in (oauth, oauthcreds)

-- | Get your API credentials.
getCreds :: Monad m => YelpT m Credentials
getCreds = yCreds `liftM` Y ask

search :: (MonadBaseControl IO m, MonadResource m) => ByteString -> ByteString -> YelpT m ByteString
search term location = do
    let req = yreq "/v2/search" [("term",term), ("location",location)]
    res <- yhttp req
    body <- asBS res
    return body

-- | A plain 'H.Request' to a Yelp API.
yreq :: Text -> HT.SimpleQuery -> H.Request n
yreq path query =
    H.def { H.host = "api.yelp.com"
          , H.path = encodeUtf8 path
          , H.queryString = HT.renderSimpleQuery False query
          , H.responseTimeout = Just 120000000 -- 2 minutes
          }

-- | Same as 'H.http', but takes care of all the boilerplate
-- (such as signing the request).
yhttp :: (MonadBaseControl IO m, MonadResource m) => 
         H.Request m 
      -> YelpT m (H.Response (C.ResumableSource m ByteString))
yhttp req = do
    manager <- yManager `liftM` Y ask
    (oauth, cred) <- yOAuth `liftM` Y ask
    lift $ do
        req' <- OA.signOAuth oauth cred req
        res <- H.http req' manager
        return res

asBS :: Monad m =>
        H.Response (C.ResumableSource m ByteString)
     -> YelpT m ByteString
asBS response = lift $ H.responseBody response C.$$+- fmap B.concat CL.consume
