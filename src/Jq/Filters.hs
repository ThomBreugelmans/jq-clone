module Jq.Filters where

data Filter = 
  Identity               | 
  Group         [Filter] |
  ObjectIndex   String   | 
  Optional      Filter   | 
  ArrayIndex    Int      | 
  ArraySlice    Int Int  | 
  ArrayValueIterator [Int]    |
  ObjectValueIterator [String]    |
  Comma Filter  Filter   | 
  Pipe Filter   Filter   |
  RecDescent             |
  
-- BONUS
  Equals        Filter Filter |
  NotEquals        Filter Filter |
  
--  ConstructValue  Filter   |
  FNull                  |
  FNum          Int      |
  FFloat        Float    |
  FBool         Bool     |
  FString       String   |
  FArray        [Filter] |
  FObject       [(Filter, Filter)]

instance Show Filter where
  show Identity = "."
  show (Group xs) = '(' : foldl (\a b -> a ++ show b) "" xs ++ ")"
  show (ObjectIndex s) = "[\"" ++ s ++ "\"]"
  show (ArrayIndex i) = "[" ++ show i ++ "]"
  show (ArraySlice s e) = "[" ++ show s ++ ":" ++ show e ++ "]"
  show (ArrayValueIterator []) = "[]"
  show (ArrayValueIterator xs) = '(' :  (tail . tail) (foldl (\a b -> a ++ ", " ++ (show b)) "" xs) ++ ")"
  show (ObjectValueIterator []) = "[]"
  show (ObjectValueIterator xs) = '(' :  (tail . tail) (foldl (\a b -> a ++ ", " ++ (show b)) "" xs) ++ ")"
  show (Comma f1 f2) = show f1 ++ ", " ++ show f2
  show (Pipe f1 f2) = show f1 ++ ", " ++ show f2
  show (Optional f) = show f ++ "?"
  show (RecDescent) = ".."
  show (Equals f1 f2) = show f1 ++ " == " ++ show f2
  show (NotEquals f1 f2) = show f1 ++ " != " ++ show f2

instance Eq Filter where
  Identity == Identity = True
  (Group as) == (Group bs) = as == bs 
  (ObjectIndex a) == (ObjectIndex b) = a == b
  (ArrayIndex a) == (ArrayIndex b) = a == b
  (ArraySlice a1 a2) == (ArraySlice b1 b2) = a1 == b1 && a2 == b2
  (ArrayValueIterator as) == (ArrayValueIterator bs) = as == bs
  (ObjectValueIterator as) == (ObjectValueIterator bs) = as == bs
  (Comma a1 a2) == (Comma b1 b2) = a1 == b1 && a2 == b2
  (Pipe a1 a2) == (Pipe b1 b2) = a1 == b1 && a2 == b2
  (Optional a) == (Optional b) = a == b
  (RecDescent) == (RecDescent) = True
  (Equals a1 a2) == (Equals b1 b2) = a1 == b1 && a2 == b2
  (NotEquals a1 a2) == (NotEquals b1 b2) = a1 == b1 && a2 == b2

--  (ConstructValue a) == (ConstructValue b) = a == b
  (FNull) == (FNull) = True
  (FNum a) == (FNum b) = a == b
  (FFloat a) == (FFloat b) = a == b
  (FBool a) == (FBool b) = a == b
  (FString a) == (FString b) = a == b
  (FArray xs) == (FArray ys) = xs == ys
  (FObject xkvs) == (FObject ykvs) = xkvs == ykvs
  _ == _ = False
  

data Config = ConfigC {filters :: Filter}

-- Smart constructors
-- Don't change the names or signatures, only the definitions

filterIdentitySC :: Filter
filterIdentitySC = Identity

filterIndexingSC :: String -> Filter
filterIndexingSC = ObjectIndex

filterPipeSC :: Filter -> Filter -> Filter
filterPipeSC = Pipe

filterCommaSC :: Filter -> Filter -> Filter
filterCommaSC = Comma
