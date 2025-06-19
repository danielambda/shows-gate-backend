module ShowsGate.DB (MonadPgConn(..), runPg) where

import Database.Beam.Postgres (Connection, Pg, runBeamPostgres)
import Control.Monad.IO.Class (MonadIO (liftIO))

class MonadIO m => MonadPgConn m where
  askPgConn :: m Connection

runPg :: MonadPgConn m => Pg a -> m a
runPg pg = do
  conn <- askPgConn
  liftIO $ runBeamPostgres conn pg


