{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}

module ShowsGate.DB.Models
  ( TitleType(..)
  , TitleT(..), Title
  , MovieT(..), Movie
  , SeriesT(..), Series
  , EpisodeT(..), Episode
  , ShowsGateDB(..), showsGateDB, checkedShowsGateDB
  ) where

import Database.Beam
import Database.Beam.Backend.SQL (HasSqlValueSyntax(..))
import Database.Beam.Migrate (CheckedDatabaseSettings, HasDefaultSqlDataType (..), defaultMigratableDbSettings, unCheckDatabase)
import Database.Beam.Postgres.CustomTypes (IsPgCustomDataType(..), pgBoundedEnumSchema, pgEnumValueSyntax, pgParseEnum)
import Database.Beam.Postgres (Postgres, )
import Database.Beam.Postgres.Syntax (PgValueSyntax)
import Data.Text (Text)
import Data.UUID (UUID)

import Data.Int (Int32)
import Data.Proxy (Proxy(..))

data TitleType
  = MovieType
  | SeriesType
  | EpisodeType
  deriving (Show, Eq, Enum, Bounded)

instance HasSqlEqualityCheck Postgres TitleType

titleTypeToString :: TitleType -> String
titleTypeToString = \case
  MovieType -> "movie"
  SeriesType -> "series"
  EpisodeType -> "series_episode"

instance FromBackendRow Postgres TitleType where
  fromBackendRow = pgParseEnum titleTypeToString

instance HasSqlValueSyntax PgValueSyntax TitleType where
  sqlValueSyntax = pgEnumValueSyntax titleTypeToString

instance IsPgCustomDataType TitleType where
  pgDataTypeName _ = "title_type"
  pgDataTypeDescription = pgBoundedEnumSchema

instance HasDefaultSqlDataType Postgres TitleType where
  defaultSqlDataType _ = defaultSqlDataType $ Proxy @Text

data TitleT f = Title
  { titleId :: Columnar f UUID
  , title :: Columnar f Text
  , titleType :: Columnar f TitleType
  } deriving (Generic, Beamable)

type Title = TitleT Identity
deriving instance Show Title
deriving instance Eq Title

instance Table TitleT where
  newtype PrimaryKey TitleT f = TitleId (Columnar f UUID)
    deriving Generic
    deriving anyclass Beamable
  primaryKey = TitleId . titleId

data MovieT f = Movie
  { movieTitleId :: PrimaryKey TitleT f
  , movieRuntimeMinutes :: Columnar (Nullable f) Int32
  , movieReleaseYear :: Columnar (Nullable f) Int32
  } deriving (Generic, Beamable)

type Movie = MovieT Identity

instance Table MovieT where
  newtype PrimaryKey MovieT f = MovieId (PrimaryKey TitleT f)
    deriving Generic
    deriving anyclass Beamable
  primaryKey = MovieId . movieTitleId

data SeriesT f = Series
  { seriesTitleId :: PrimaryKey TitleT f
  , seriesStartYear :: Columnar (Nullable f) Int32
  , seriesEndYear :: Columnar (Nullable f) Int32
  } deriving (Generic, Beamable)

type Series = SeriesT Identity

instance Table SeriesT where
  newtype PrimaryKey SeriesT f = SeriesId (PrimaryKey TitleT f)
    deriving Generic
    deriving anyclass Beamable
  primaryKey = SeriesId . seriesTitleId

data EpisodeT f = Episode
  { episodeTitleId :: PrimaryKey TitleT f
  , episodeSeriesTitleId :: PrimaryKey SeriesT f
  , episodeSeasonNumber :: Columnar f Int32
  , episodeNumber :: Columnar f Int32
  } deriving (Generic, Beamable)

type Episode = EpisodeT Identity

instance Table EpisodeT where
  newtype PrimaryKey EpisodeT f = EpisodeId (PrimaryKey TitleT f)
    deriving Generic
    deriving anyclass Beamable
  primaryKey = EpisodeId . episodeTitleId

data ShowsGateDB f = ShowsGateDB
  { titles :: f (TableEntity TitleT)
  , movies :: f (TableEntity MovieT)
  , series :: f (TableEntity SeriesT)
  , episodes :: f (TableEntity EpisodeT)
  } deriving (Generic, Database be)

checkedShowsGateDB :: CheckedDatabaseSettings Postgres ShowsGateDB
checkedShowsGateDB = defaultMigratableDbSettings

showsGateDB :: DatabaseSettings Postgres ShowsGateDB
showsGateDB = unCheckDatabase checkedShowsGateDB
