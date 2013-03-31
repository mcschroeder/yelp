{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.Yelp 
    ( YelpT
    , runYelpT    
    , Credentials(..)
    , getCreds
    ) where

import Data.ByteString (ByteString)
import Network.HTTP.Conduit (Manager, withManager)
import Control.Monad (liftM)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)

-- | Yelp API access credentials.
data Credentials = 
    Credentials { consumerKey    :: ByteString
                , consumerSecret :: ByteString
                , token          :: ByteString
                , tokenSecret    :: ByteString
                } deriving (Show)

-- | @YelpT m a@ is a monad transformer containing 
-- information needed to use the Yelp API.
newtype YelpT m a = YelpT { unY :: ReaderT YData m a }
    deriving (Monad, MonadIO)

-- | Internal data of 'YelpT'.
data YData = YData { yCreds   :: Credentials 
                   , yManager :: Manager
                   }

-- | Run a computation in the 'YelpT' monad transformer 
-- with your credentials.
runYelpT :: Credentials     -- ^ Your app's credentials.
         -> Manager         -- ^ Connection manager (see 'withManager')
         -> YelpT m a
         -> m a
runYelpT creds manager (YelpT act) = 
    runReaderT act (YData creds manager)

-- | Get your API credentials.
getCreds :: Monad m => YelpT m Credentials
getCreds = yCreds `liftM` YelpT ask
