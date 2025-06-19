module ShowsGate.Domain.Movie (Movie(..)) where

import Data.Time (Year)

import ShowsGate.Domain.Title (Title, TitleId)
import ShowsGate.Domain.Movie.Primitives (MovieRuntime)

data Movie = Movie
  { titleId :: TitleId
  , title :: Title
  , mRuntimeMinutes :: Maybe MovieRuntime
  , mReleaseYear :: Maybe Year
  }

