{-# LANGUAGE DeriveGeneric #-}
-- | Test data types for TH schema generation tests.
-- These are in a separate module because TH splices can only see
-- definitions from earlier in the module or from imported modules.
module Text.Ginger.TH.TestTypes where

import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)

-- Simple record
data SimpleRecord = SimpleRecord
  { srName :: Text
  , srAge :: Int
  } deriving (Show, Eq, Generic)

-- Nested record
data NestedRecord = NestedRecord
  { nrUser :: SimpleRecord
  , nrActive :: Bool
  } deriving (Show, Eq, Generic)

-- Sum type
data ContentType
  = TextContent { ctBody :: Text }
  | ImageContent { ctUrl :: Text, ctAlt :: Text }
  deriving (Show, Eq, Generic)

-- Sum type with shared field
data Animal
  = Dog { animalName :: Text, animalBreed :: Text }
  | Cat { animalName :: Text, animalColor :: Text }
  deriving (Show, Eq, Generic)

-- Recursive type
data Tree = Node
  { treeValue :: Int
  , treeChildren :: [Tree]
  } deriving (Show, Eq, Generic)

-- Record with list
data WithList = WithList
  { wlItems :: [Text]
  , wlCount :: Int
  } deriving (Show, Eq, Generic)

-- Record with Maybe
data WithMaybe = WithMaybe
  { wmRequired :: Text
  , wmOptional :: Maybe Text
  } deriving (Show, Eq, Generic)

-- Record with Vector
data WithVector = WithVector
  { wvItems :: Vector Text
  } deriving (Show, Eq, Generic)

-- Newtype wrapper
newtype UserId = UserId { unUserId :: Int }
  deriving (Show, Eq, Generic)

-- Type synonym (used in a record)
type Email = Text

data WithTypeSynonym = WithTypeSynonym
  { wtsEmail :: Email
  , wtsName :: Text
  } deriving (Show, Eq, Generic)
