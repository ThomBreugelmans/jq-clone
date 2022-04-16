module Jq.Compiler where

import           Jq.Filters
import           Jq.Json
import Data.Either (isRight, fromRight, fromLeft)


type JProgram a = JSON -> Either String a

compile :: Filter -> JProgram [JSON]
compile Identity inp = return [inp]

compile (Group fs) inp = compile (foldl (\a b -> (Pipe a b)) Identity fs) inp

compile (ObjectIndex i) (JObject kvs) = case lookup i kvs of
  Just v  -> Right [v]
  Nothing -> Right [JNull]
compile (ObjectIndex _) JNull = Right [JNull]
compile (ObjectIndex _) inp = Left ("Input is not of type JObject: " ++ show inp)

compile (ArrayIndex 0) (JArray (x:xs)) = Right [x]
compile (ArrayIndex 0) (JArray []) = Left "Index out of bounds"
compile (ArrayIndex i) (JArray (_:xs)) = compile (ArrayIndex (i-1)) (JArray xs)

compile (ArraySlice 0 x) (JArray [])
  | x <= 0 = Right [JArray []]
  | otherwise = Left "Slice out of bounds"
compile (ArraySlice 0 1) (JArray (x:xs)) = Right [JArray [x]]
compile (ArraySlice 0 e) (JArray (x:xs)) = case compile (ArraySlice 0 (e-1)) (JArray xs) of
  Right [JArray ys] -> Right [JArray (x : ys)]
  err -> err
compile (ArraySlice s e) (JArray (x:xs)) = compile (ArraySlice (s-1) (e-1)) (JArray xs)

compile (ValueIterator []) (JArray _) = Right [JArray []]
compile (ValueIterator _) (JArray []) = Left "ValueIterator index out of bounds"
compile (ValueIterator (0:vs)) (JArray (x:xs)) = case compile (ValueIterator (map (\x->x-1) vs)) (JArray xs) of
  Right [JArray ys] -> Right [JArray (x:ys)]
  err -> err
compile (ValueIterator vs) (JArray (_:xs)) = compile (ValueIterator (map (\x->x-1) vs)) (JArray xs)
compile (ValueIterator []) (JObject _) = Right [JArray []]
compile (ValueIterator _) (JObject []) = Left "ValueIterator index out of bounds"
compile (ValueIterator (0:vs)) (JObject ((_,x):xs)) = case compile (ValueIterator (map (\x->x-1) vs)) (JObject xs) of
  Right [JArray ys] -> Right [JArray (x:ys)]
  err -> err
compile (ValueIterator vs) (JObject (_:xs)) = compile (ValueIterator (map (\x->x-1) vs)) (JObject xs)

compile (Optional Identity) _ = Left "No Optional possible for filter of type \"Identity\""
compile (Optional (Group _)) _ = Left "No Optional possible for filter of type \"Group\""
compile (Optional (Comma _ _)) _ = Left "No Optional possible for filter of type \"Comma\""
compile (Optional (Pipe _ _)) _ = Left "No Optional possible for filter of type \"Pipe\""
compile (Optional f) inp = case compile f inp of
  Left _ -> Right []
  res -> res

compile (Comma a b) inp = either Left (\av -> either Left (\bv -> Right (bv ++ av)) (compile b inp)) (compile a inp)

compile (Pipe a b) inp = case compile a inp of
  Left err -> Left err
  Right out -> foldr
                  (\ x res ->
                    either
                      Left
                      (\outp ->
                        if isRight res then
                          Right (fromRight [] res ++ outp)
                        else res)
                      (compile b x))
                  (Right [])
                  out

compile f i = Left ("Error, provided filter: " ++ show f ++ " and input: " ++ show i ++ " do not match!")

run :: JProgram [JSON] -> JSON -> Either String [JSON]
run p j = p j
