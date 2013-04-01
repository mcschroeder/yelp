{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}

module Web.Yelp.Monad 
    ( YelpT
    , runYelpT
    , getManager
    , getOAuth

      -- * Re-export
    , lift
    ) where

import Control.Applicative (Applicative)
import Control.Monad (liftM)
import Control.Monad.Base (MonadBase(..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Control ( MonadTransControl(..)
                                   , MonadBaseControl(..)
                                   , ComposeSt, defaultLiftBaseWith
                                   , defaultRestoreM )
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans.Resource (MonadThrow, MonadResource)
import qualified Network.HTTP.Conduit as H
import qualified Web.Authenticate.OAuth as OA

import Web.Yelp.Types

-- | @YelpT m a@ is a monad transformer containing 
-- information needed to use the Yelp API.
newtype YelpT m a = Y { unY :: ReaderT YData m a }
    deriving ( Functor, Applicative, Monad
             , MonadIO, MonadTrans
             , MonadThrow, MonadResource )

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
data YData = YData { yManager    :: H.Manager
                   , yOAuth      :: (OA.OAuth, OA.Credential)
                   }

-- | Run a computation in the 'YelpT' monad transformer 
-- with your credentials.
runYelpT :: Credentials     -- ^ Your app's credentials.
         -> H.Manager       -- ^ Connection manager (see 'H.withManager')
         -> YelpT m a
         -> m a
runYelpT creds manager (Y act) = 
    runReaderT act (YData manager (credsToOAuth creds))

credsToOAuth :: Credentials -> (OA.OAuth, OA.Credential)
credsToOAuth (Credentials ckey csec tok toksec) = 
    let oauth = OA.newOAuth { OA.oauthConsumerKey    = ckey
                            , OA.oauthConsumerSecret = csec
                            }
        oauthcreds = OA.newCredential tok toksec
    in (oauth, oauthcreds)

-- | Get the 'H.Manager'.
getManager :: Monad m => YelpT m H.Manager
getManager = yManager `liftM` Y ask

-- | Get OAuth related data.
getOAuth :: Monad m => YelpT m (OA.OAuth, OA.Credential)
getOAuth = yOAuth `liftM` Y ask
