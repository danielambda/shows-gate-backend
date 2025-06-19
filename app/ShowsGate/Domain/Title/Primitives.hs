module ShowsGate.Domain.Title.Primitives (TitleId(..)) where

import Data.UUID (UUID)

newtype TitleId = TitleId { value :: UUID }

