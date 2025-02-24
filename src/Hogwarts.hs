{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hogwarts (getDB, getActor, getPatronus, getMugglebornNames, getHouseCounts) where

import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LB
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

personName :: Person -> String
personName (Person {..}) = name

personPatronus :: Person -> String
personPatronus (Person {..}) = patronus

personActor :: Person -> String
personActor (Person {..}) = actor

personHouse :: Person -> String
personHouse (Person {..}) = house

-- | vrátí zdali je postava z mudlovské rodiny
-- v ukázkovém výstupu getMuggleborn je 'Lilly Potter' jež je half-blood
isMuggle :: Person -> Bool
isMuggle (Person {..}) = bornType == "muggleborn" || bornType == "half-blood"

-- | json url
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
      pure (Right $ LB.fromStrict result) -- TODO: Upravit importy aby nebylo třeba convertovat z Lazy na strict

-- | Interní funkce pro konverzi z listu postav na mapu
-- kde klíč je jméno postavy a hodnotou postava
parseToDB :: [Person] -> Map String Person
parseToDB = foldl go M.empty
  where
    go :: Map String Person -> Person -> Map String Person
    go b a = M.insert (personName a) a b

-- | Vrátí databází postav nebo string s chybou
getDB :: IO (Either String DB)
getDB = do
  res <- fetchJSON
  case res of
    (Left err) -> pure $ Left err
    (Right cont) -> do
      case eitherDecode cont of
        (Right out) -> pure $ Right (parseToDB out)
        (Left err) -> pure $ Left err

-- | Vrátí herce pro danou postavu
getActor :: String -> DB -> Maybe String
getActor key = maybe Nothing (Just . personActor) . M.lookup key

-- | Vrátí patrona pro danou postavu
getPatronus :: String -> DB -> Maybe String
getPatronus key = maybe Nothing (Just . personPatronus) . M.lookup key

-- | Vrátí všechny postavy z mudlovské rodiny
getMugglebornNames :: DB -> [String]
getMugglebornNames = foldl go []
  where
    go :: [String] -> Person -> [String]
    go b a
      | isMuggle a = personName a : b
      | otherwise = b

-- | Vrátí list dvojic (jméno koleje, počet studentů)
getHouseCounts :: DB -> [(String, Int)]
getHouseCounts = M.toList . foldr go M.empty
  where
    go :: Person -> Map String Int -> Map String Int
    go p =
      let house = personHouse p
       in if house /= ""
            then M.alter msucc house
            else id
    msucc :: Maybe Int -> Maybe Int
    msucc = Just . maybe 1 (+ 1)
