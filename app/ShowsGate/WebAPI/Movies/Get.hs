module ShowsGate.WebAPI.Movies.Get (getMovie) where

import Database.Beam
import Database.Beam.Postgres (Postgres)
import Data.Text (Text)
import Data.Validation (Validation, validationNel, validation)
import Servant (ServerT, respond, WithStatus (..))

import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty)

import ShowsGate.Contracts.Movies (MovieResp (..), GetMovie, MovieNotFound (..))
import ShowsGate.Domain.Movie (Movie (..))
import ShowsGate.Domain.Movie.Primitives (mkMovieRuntime)
import ShowsGate.Domain.Title (TitleId(..), mkTitle)
import ShowsGate.DB (MonadPgConn, runPg)
import ShowsGate.DB.Models (TitleType(..), ShowsGateDB(..), showsGateDB, MovieT (movieTitleId))
import qualified ShowsGate.DB.Models as DB

movieToResp :: Movie -> MovieResp
movieToResp movie = MovieResp
  { titleId = movie.titleId.value
  , title = movie.title.value
  , runtimeMinutes = movie.mRuntimeMinutes <&> (.value)
  , releaseYear = fromIntegral <$> movie.mReleaseYear
  }

getMovie :: MonadPgConn m => ServerT GetMovie m
getMovie titleId = do
  getMovieFromDb (TitleId titleId) >>= \case
    Nothing -> respond MovieNotFound
    Just movie -> respond $ WithStatus @200 $ movieToResp movie

selectOne
  :: (MonadPgConn m, FromBackendRow Postgres (QExprToIdentity res), Projectible Postgres res)
  => Q Postgres db QBaseScope res -> m (Maybe (QExprToIdentity res))
selectOne query = runPg $ runSelectReturningOne $ select query


getMovieFromDb :: MonadPgConn m => TitleId -> m (Maybe Movie)
getMovieFromDb (TitleId titleId) = do
  results <- selectOne $ do
    title <- filter_ (\title -> title.titleId ==. val_ titleId
                            &&. title.titleType ==. val_ MovieType)
           $ all_ showsGateDB.titles
    movie <- oneToOne_ showsGateDB.movies movieTitleId title
    return (title, movie)
  return $ validation (const Nothing) Just . movieFromDB =<< results

movieFromDB :: (DB.Title, DB.Movie) -> Validation (NonEmpty Text) Movie
movieFromDB (dbTitle, dbMovie) = do
  let titleId = TitleId dbTitle.titleId
  title <- validationNel $ mkTitle dbTitle.title
  mRuntimeMinutes <- traverse
    (validationNel . mkMovieRuntime . fromIntegral)
    dbMovie.movieRuntimeMinutes
  let mReleaseYear = fromIntegral <$> dbMovie.movieReleaseYear
  return Movie{..}

