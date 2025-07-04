module ShowsGate.WebAPI where

import Servant

import ShowsGate.WebAPI.Movies (moviesServer)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Error.Class (MonadError)
import qualified Data.UUID.V4 as V4 (nextRandom)
import Control.Monad.Reader (asks, MonadReader, ReaderT (..))
import Data.Pool (Pool, withResource)
import qualified Database.Beam.Postgres as Pg (Connection)
import ShowsGate.DB (MonadPgConn (..))
import ShowsGate.Contracts (API)
import ShowsGate.WebAPI.Common.MonadUUID (MonadUUID (..))

newtype Env = Env { pgConnPool :: Pool Pg.Connection }

newtype AppM a = AppM { unAppM :: ReaderT Env Handler a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadError ServerError)

instance MonadUUID AppM where newUUID = liftIO V4.nextRandom

instance MonadPgConn AppM where
  askPgConn = do
    pool <- asks pgConnPool
    liftIO $ withResource pool return

server :: ServerT API AppM
server = moviesServer

mkApp :: Env -> Application
mkApp env =
  serve api $ hoistServer api nt server
  where
    api = Proxy :: Proxy API

    nt :: AppM a -> Handler a
    nt = flip runReaderT env . unAppM

