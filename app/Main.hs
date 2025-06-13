{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.Beam.Migrate.Simple (createSchema)
import Database.Beam.Postgres (connectPostgreSQL, runBeamPostgres, close)
import Database.Beam.Postgres.Migrate (migrationBackend)
import qualified Data.ByteString.Char8 as BS (pack)

import Control.Exception (bracket)

import DatabaseModels (showsGateDB)
import System.Environment (getEnv)

initializeTables :: IO ()
initializeTables = do
  connStr <- BS.pack <$> getEnv "POSTGRESQL_CONNECTION_STRING"
  bracket (connectPostgreSQL connStr) close $ \conn ->
    runBeamPostgres conn $ createSchema migrationBackend showsGateDB
  putStrLn "Tables initialization completed successfully!"

main :: IO ()
main = do
  pure ()
