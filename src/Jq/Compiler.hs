module Jq.Compiler where

import           Jq.Filters
import           Jq.Json
import Data.Either (isRight, fromRight, fromLeft)
import Data.Map (lookup, elems, notMember, fromList, empty)
import Data.Maybe (fromJust)


type JProgram a = JSON -> Either String a

compile :: Filter -> JProgram [JSON]
compile Identity inp = return [inp]

compile (Group fs) inp = compile (foldl (\a b -> (Pipe a b)) Identity fs) inp

compile (ObjectIndex i) (JObject map) = case Data.Map.lookup i map of
  Just v  -> Right [v]
  Nothing -> Right [JNull]
compile (ObjectIndex _) JNull = Right [JNull]
compile (ObjectIndex _) inp = Left ("Input is not of type JObject: " ++ show inp)

compile (ArrayIndex 0) (JArray (x:xs)) = Right [x]
compile (ArrayIndex 0) (JArray []) = Left "Index out of bounds"
compile (ArrayIndex i) (JArray (x:xs))
  | i > 0 = compile (ArrayIndex (i-1)) (JArray xs)
  | i < 0 = compile (ArrayIndex ((-i)-1)) (JArray $ reverse (x:xs))

compile (ArraySlice s e) (JArray xs)
  | s < 0 = compile (ArraySlice (length xs - min (-s) (length xs)) e) (JArray xs)
  | e < 0 = compile (ArraySlice s (length xs - min (-e) (length xs))) (JArray xs)
  | xs == [] = Right [JArray []]
  | s >= e = Right [JArray []]
  | s == e = Right [JArray []]
  | s == 0 = case compile (ArraySlice s (e-1)) (JArray (tail xs)) of
    Right [JArray ys] -> Right [JArray (head xs : ys)]
    err -> err
  | otherwise = compile (ArraySlice (s-1) (e-1)) (JArray $ tail xs)


compile (ArrayValueIterator []) (JArray xs) = Right xs
compile (ArrayValueIterator []) (JObject mp) = compile (ObjectValueIterator []) (JObject mp)
compile (ArrayValueIterator (v:vs)) (JArray xs)
  | v < 0           = compile (ArrayValueIterator ((length xs + v):vs)) (JArray xs)
  | vs /= []        = (f :) <$> compile (ArrayValueIterator vs) (JArray xs)
  | otherwise       = Right [f]
  where
    f
      | v >= length xs  = JNull
      | otherwise       = xs !! v
compile (ObjectValueIterator []) (JObject kvs) = Right $ elems kvs
compile (ObjectValueIterator []) (JArray xs) = compile (ArrayValueIterator []) (JArray xs)
compile (ObjectValueIterator (v:vs)) (JObject kvs)
  | vs /= []        = (f :) <$> compile (ObjectValueIterator vs) (JObject kvs)
  | otherwise       = Right [f]
  where
    f
      | notMember v kvs   = JNull
      | otherwise         = fromJust $ Data.Map.lookup v kvs

compile (Optional Identity) _ = Left "No Optional possible for filter of type \"Identity\""
compile (Optional (Group _)) _ = Left "No Optional possible for filter of type \"Group\""
compile (Optional (Comma _ _)) _ = Left "No Optional possible for filter of type \"Comma\""
compile (Optional (Pipe _ _)) _ = Left "No Optional possible for filter of type \"Pipe\""
compile (Optional f) inp = case compile f inp of
  Left _ -> Right []
  res -> res

compile (Comma a b) inp = either Left (\av -> either Left (\bv -> Right (av ++ bv)) (compile b inp)) (compile a inp)

compile (Pipe a b) inp = case compile a inp of
  Left err -> Left err
  Right av -> f av
  where
    f [] = Right []
    f (x:xs) = either Left (\out -> (out ++) <$> f xs) (compile b x)


compile RecDescent (JObject mp) = Right $ JObject mp : elems mp
  where
    c :: [JSON] -> [JSON]
    c [] = []
    c (y:ys) = fromRight [] (compile RecDescent y) ++ c ys
compile RecDescent (JArray []) = Right [JArray []]
compile RecDescent (JArray xs) = Right $ JArray xs : c xs
  where
    c :: [JSON] -> [JSON]
    c [] = []
    c (y:ys) = fromRight [] (compile RecDescent y) ++ c ys
compile RecDescent inp = Right [inp]
                  
--compile (ConstructValue (FBool b)) inp = Right [JBool b]
--compile (ConstructValue (FNum f)) inp = Right [JFloat f]
compile (FNull) inp = Right [JNull]
compile (FBool b) inp = Right [JBool b]
compile (FNum f) inp = Right [JNum f]
compile (FFloat f) inp = Right [JFloat f]
compile (FString s) inp = Right [JString s]
compile (FArray []) inp = Right [JArray []]
compile (FArray (x:xs)) inp = case compile (FArray xs) inp of
  Right res -> concat . (map (\v -> map (\(JArray ys) -> JArray (v:ys)) res)) <$> (compile x inp)
  err -> err
compile (FObject kvs) inp = Right [JObject (fromList (map (\(k,v) -> (compileKey k, compileVal v)) kvs))]
  where
    compileKey k = case compile k inp of
      Right [JString s] -> s
    compileVal v = case compile v inp of
      Right [x] -> x

compile f i = Left ("Error, provided filter: " ++ show f ++ " and input: " ++ show i ++ " do not match!")

run :: JProgram [JSON] -> JSON -> Either String [JSON]
run p j = p j
