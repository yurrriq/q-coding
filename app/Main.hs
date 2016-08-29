module Main where

import           Data.Aeson
import qualified Data.ByteString.Lazy as B
import           Data.WakaTime.Types

main :: IO ()
main = do
  d <- (eitherDecode <$> getJSON) :: IO (Either String Heartbeats)
  case d of
    Left  err -> putStrLn err
    Right hb  -> print hb

jsonFile :: FilePath
jsonFile = "data/heartbeats/2016-07-03.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile
