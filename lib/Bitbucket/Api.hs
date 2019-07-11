{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Bitbucket.Api where

import Control.Lens
import Data.Aeson.Types
import Data.Aeson
import GHC.Generics (Generic)
import Data.Text hiding (drop)

data Page a = Page
  { _isLastPage :: !Bool
  , _limit      :: !Int
  , _size       :: !Int
  , _values     :: [a]
  } deriving (Show, Generic)

makeLenses ''Page

instance (FromJSON a) => FromJSON (Page a) where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 1}

instance (ToJSON a) => ToJSON (Page a) where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = drop 1}

-- decode "{\"isLastPage\": true, \"limit\": 10, \"size\": 5, \"values\": [1,2,3]}" :: Maybe (Page Int)

data Reference = Reference
  { _displayId :: !Text
  } deriving (Show, Generic)

makeLenses ''Reference

instance FromJSON Reference where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 1}

instance ToJSON Reference where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = drop 1}


data PullRequest = PullRequest
  { _id :: !Int
  , _title :: !Text
  , _toRef :: !Reference
  , _links :: !Value
  } deriving (Show, Generic)

makeLenses ''PullRequest

instance FromJSON PullRequest where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 1}

instance ToJSON PullRequest where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = drop 1}
