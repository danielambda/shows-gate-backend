{-# LANGUAGE OverloadedStrings #-}

module ShowsGate.Domain.Movie.Primitives (MovieRuntime, mkMovieRuntime) where

import Data.Text (Text)

import GHC.Records (HasField (getField))

newtype MovieRuntime = MovieRuntime { _value :: Int }

mkMovieRuntime :: Int -> Either Text MovieRuntime
mkMovieRuntime n
  | n < 0 = Left "Movie runtime cannot be negative"
  | otherwise = Right $ MovieRuntime n

instance HasField "value" MovieRuntime Int where
  getField = _value
