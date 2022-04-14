module Jq.Json where

data JSON =
    JNull           |
    JNum    Int     |
    JString String  |
    JBool   Bool    |
    JArray  [JSON]  |
    JObject [(String, JSON)]

instance Show JSON where
  show (JNull) = "null"
  show (JNum a) = show a
  show (JString s) = '\"' : s ++ "\""
  show (JBool True) = "true"
  show (JBool False) = "false"
  show (JArray []) = "[]"
  show (JArray xs) = "[\n" ++ unlines (map (\x -> "  " ++ (show x)) xs) ++ "]"
  show (JObject []) = "{}"
  show (JObject kvs) = "{\n" ++ unlines (map (\(key, val) -> "  " ++ (show key) ++ ": " ++ (show val)) kvs) ++ "}"
  show _ = undefined

instance Eq JSON where
  (JNull) == (JNull) = True
  (JNum a) == (JNum b) = a == b
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
jsonNumberSC x = JNum x

jsonStringSC :: String -> JSON
jsonStringSC s = JString s

jsonBoolSC :: Bool -> JSON
jsonBoolSC b = JBool b

jsonArraySC :: [JSON] -> JSON
jsonArraySC js = JArray js

jsonObjectSC :: [(String, JSON)] -> JSON
jsonObjectSC key_values = JObject key_values
