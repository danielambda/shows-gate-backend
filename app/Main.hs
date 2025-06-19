{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.Beam.Migrate.Simple (createSchema)
import Database.Beam.Postgres (connectPostgreSQL, runBeamPostgres)
import Database.Beam.Postgres.Migrate (migrationBackend)
import qualified Data.ByteString.Char8 as BS (pack)

import Control.Exception (bracket, finally)
import System.Environment (getEnv)

import ShowsGate.DB.Models (checkedShowsGateDB)
import ShowsGate.WebAPI
import Network.Wai.Handler.Warp (runSettings, defaultSettings, setHost, setPort, setBeforeMainLoop)
import Data.Function ((&))
import System.IO (hPutStrLn, stderr)
import qualified Data.ByteString.Char8 as BS8
import Data.Pool (newPool, defaultPoolConfig, destroyAllResources, Pool)
import qualified Database.Beam.Postgres as Pg

initializeTables :: IO ()
initializeTables = do
  connStr <- BS.pack <$> getEnv "POSTGRESQL_CONNECTION_STRING"
  bracket (connectPostgreSQL connStr) Pg.close $ \conn ->
    runBeamPostgres conn $ createSchema migrationBackend checkedShowsGateDB
  putStrLn "Tables initialization completed successfully!"

runAPI :: Pool Pg.Connection -> IO ()
runAPI pgConnPool = do
  let app = mkApp $ Env pgConnPool
  runSettings settings app
  where
    port = 8080
    settings = defaultSettings
      & setHost "0.0.0.0"
      & setPort port
      & setBeforeMainLoop (hPutStrLn stderr $ "listening on port " <> show port)

main :: IO ()
main = do
  pgConn <- connectPostgreSQL . BS8.pack <$> getEnv "POSTGRESQL_CONNECTION_STRING"
  pgConnPool <- newPool $ defaultPoolConfig pgConn Pg.close
                            20 -- Keepalive time (seconds)
                            50 -- Max resourcesction is kept

  runAPI pgConnPool
    `finally` do
      destroyAllResources pgConnPool
