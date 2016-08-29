{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.WakaTime.Types -- where
  ( Heartbeats(..)
  , Heartbeat(..)
  ) where

import           Control.Monad (mzero)
import           Data.Aeson
import           Data.Text     (Text)
import           GHC.Generics  (Generic)

-- | A day's worth of 'Heartbeat's.
data Heartbeats = Heartbeats
  { -- | @"data"@ in JSON.
    heartbeats :: [Heartbeat]
    -- | e.g. @"2016-07-06T04:59:59Z"@
  , end        :: Text
    -- | e.g. @"2016-07-05T05:00:00Z"@
  , start      :: Text
    -- | e.g. @"America/Chicago"@
  , timezone   :: Text
  } deriving (Show)

instance FromJSON Heartbeats where
  parseJSON (Object v) =
    Heartbeats <$> v .: "data"
               <*> v .: "end"
               <*> v .: "start"
               <*> v .: "timezone"
  parseJSON _ = mzero

instance ToJSON Heartbeats where
  toJSON (Heartbeats hb e s t) =
    object [ "data"     .= hb
           , "end"      .= e
           , "start"    .= s
           , "timezone" .= t
           ]

-- | Heartbeat
data Heartbeat = Heartbeat
  { branch       :: Maybe Text
  , entity       :: Text
  -- , id     :: Text
  , is_debugging :: Bool
  , is_write     :: Bool
  , language     :: Maybe Text
  , project      :: Maybe Text
  , time         :: Double      -- FIXME: Float?
  -- , type         :: Text
  }
  deriving (Generic, Show)

instance FromJSON Heartbeat
instance ToJSON   Heartbeat
