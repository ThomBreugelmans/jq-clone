module Jq.Filters where

data Filter = 
  Identity               | 
  Group         [Filter] | 
  ObjectIndex   String   | 
  Optional      Filter   | 
  ArrayIndex    Int      | 
  ArraySlice    Int Int  | 
  ValueIterator [Int]    | 
  Comma Filter  Filter   | 
  Pipe Filter   Filter

instance Show Filter where
  show Identity = "."
  show (Group xs) = '(' : foldl (\a b -> a ++ show b) "" xs ++ ")"
  show (ObjectIndex s) = "[\"" ++ s ++ "\"]"
  show (ArrayIndex i) = "[" ++ show i ++ "]"
  show (ArraySlice s e) = "[" ++ show s ++ ":" ++ show e ++ "]"
  show (ValueIterator []) = "[]"
  show (ValueIterator xs) = '(' :  (tail . tail) (foldl (\a b -> a ++ ", " ++ (show b)) "" xs) ++ ")"
  show (Comma f1 f2) = show f1 ++ ", " ++ show f2
  show (Pipe f1 f2) = show f1 ++ ", " ++ show f2
  show (Optional f) = show f ++ "?"

instance Eq Filter where
  Identity == Identity = True
  (Group as) == (Group bs) = as == bs 
  (ObjectIndex a) == (ObjectIndex b) = a == b
  (ArrayIndex a) == (ArrayIndex b) = a == b
  (ArraySlice a1 a2) == (ArraySlice b1 b2) = a1 == b1 && a2 == b2
  (ValueIterator as) == (ValueIterator bs) = as == bs
  (Comma a1 a2) == (Comma b1 b2) = a1 == b1 && a2 == b2
  (Pipe a1 a2) == (Pipe b1 b2) = a1 == b1 && a2 == b2
  (Optional a) == (Optional b) = a == b
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
