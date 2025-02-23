module Hogwarts (getDB, getActor, getPatronus, getMugglebornNames, getHouseCounts) where

type DB = Int -- TODO

getDB :: IO (Either String DB)
getDB = undefined

getActor :: String -> DB -> Maybe String
getActor = undefined

getPatronus :: String -> DB -> Maybe String
getPatronus = undefined

getMugglebornNames :: DB -> [String]
getMugglebornNames = undefined

getHouseCounts :: DB -> [(String, Int)]
getHouseCounts = undefined
