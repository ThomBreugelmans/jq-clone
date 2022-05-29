module Jq.Json where

import Data.List (intercalate)
import Data.Map (Map, assocs, fromList, size)


data JSON =
    JNull           |
    JNum    Int     |
    JFloat  Float   |
    JString String  |
    JBool   Bool    |
    JArray  [JSON]  |
    JObject (Map String JSON)
    
prettyIndent :: String -> String 
prettyIndent [] = []
prettyIndent ('\n':inp) = "\n  " ++ prettyIndent inp
prettyIndent (x:inp) = x : prettyIndent inp

instance Show JSON where
  show (JNull) = "null"
  show (JNum a) = show a
  show (JFloat a) = show a
  show (JString s) = '\"' : s ++ "\""
  show (JBool True) = "true"
  show (JBool False) = "false"
  show (JArray []) = "[]"
  show (JArray xs) = "[\n  " ++ intercalate ",\n  " (map (prettyIndent . show) xs) ++ "\n]"
  show (JObject mp)
    | size mp == 0  = "{}"
    | otherwise     = "{\n  " ++ intercalate ",\n  " (map (\(k, v) -> prettyIndent (show k) ++ ": " ++ prettyIndent (show v)) kvs) ++ "\n}"
    where
      kvs = assocs mp

instance Eq JSON where
  (JNull) == (JNull) = True
  (JNum a) == (JNum b) = a == b
  (JFloat a) == (JFloat b) = a == b
  (JString a) == (JString b) = a == b
  (JBool a) == (JBool b) = a == b
  (JArray xs) == (JArray ys) = xs == ys
  (JObject akvs) == (JObject bkvs) = akvs == bkvs
  _ == _ = False

-- Smart constructors
-- Don't change the names or signatures, only the definitions

jsonNullSC :: JSON
jsonNullSC = JNull

jsonNumberSC :: Int -> JSON
jsonNumberSC = JNum

jsonStringSC :: String -> JSON
jsonStringSC = JString

jsonBoolSC :: Bool -> JSON
jsonBoolSC = JBool

jsonArraySC :: [JSON] -> JSON
jsonArraySC = JArray

jsonObjectSC :: [(String, JSON)] -> JSON
jsonObjectSC = JObject . fromList
