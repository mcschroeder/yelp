module Web.Yelp.Base
	( yreq
	, yhttp
	, asBS
	) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Network.HTTP.Conduit as H
import qualified Network.HTTP.Types as HT
import qualified Web.Authenticate.OAuth as OA

import Web.Yelp.Monad
import Web.Yelp.Types

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
yhttp :: (C.MonadBaseControl IO m, C.MonadResource m) => 
         H.Request m 
      -> YelpT m (H.Response (C.ResumableSource m ByteString))
yhttp req = do
    manager <- getManager
    (oauth, cred) <- getOAuth
    lift $ do
        req' <- OA.signOAuth oauth cred req
        res <- H.http req' manager
        return res

asBS :: Monad m =>
        H.Response (C.ResumableSource m ByteString)
     -> YelpT m ByteString
asBS response = lift $ H.responseBody response C.$$+- fmap B.concat CL.consume
