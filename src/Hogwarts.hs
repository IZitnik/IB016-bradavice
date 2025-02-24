{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hogwarts (getDB, getActor, getPatronus, getMugglebornNames, getHouseCounts) where

import Data.Aeson
import Data.ByteString.Lazy
import Data.Map (Map)
import qualified Data.Map as M
import GHC.Generics
import Network.HTTP.Simple
import Network.HTTP.Types.Status (statusMessage)

data Person = Person
  { name :: String,
    actor :: String,
    patronus :: String,
    bornType :: String,
    house :: String
  }
  deriving (Generic, Show)

type DB = Map String Person

instance FromJSON Person where
  parseJSON = withObject "Person" $ \o -> do
    name <- o .: "name"
    actor <- o .: "actor"
    patronus <- o .: "patronus"
    bornType <- o .: "ancestry"
    house <- o .: "house"
    return Person {..}

base_url :: String
base_url = "https://www.fi.muni.cz/~xjonas/hpcharacters.json"

fetchJSON :: IO (Either String ByteString)
fetchJSON = do
  request <- parseRequest base_url
  response <- httpBS request
  if getResponseStatusCode response /= 200
    then pure $ Left (show . statusMessage $ getResponseStatus response)
    else do
      let result = getResponseBody response
      pure (Right $ fromStrict result)

parseToDB :: [Person] -> Map String Person
parseToDB = undefined

-- | Returns database of harry potter characters or error
getDB :: IO (Either String DB)
getDB = do
  res <- fetchJSON
  case res of
    (Left err) -> pure $ Left err
    (Right cont) -> do
      case eitherDecode cont of
        (Right out) -> pure $ Right (parseToDB out)
        (Left err) -> pure $ Left err

getActor :: String -> DB -> Maybe String
getActor = undefined

getPatronus :: String -> DB -> Maybe String
getPatronus = undefined

getMugglebornNames :: DB -> [String]
getMugglebornNames = undefined

getHouseCounts :: DB -> [(String, Int)]
getHouseCounts = undefined
