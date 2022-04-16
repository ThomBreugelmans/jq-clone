module Jq.CParser where

import Parsing.Parsing
import Jq.Filters

parseIdentity :: Parser Filter
parseIdentity = do
  _ <- space *> char '.' <* space
  return Identity
  
parseGroup = undefined

parseObjectIndex :: Parser Filter
parseObjectIndex = ObjectIndex <$> (string ".[" *> space *> char '"' *> many (sat (/= '"')) <* char '"' <* space <* char ']')
  <|>
    ObjectIndex <$> (char '.' *> ident)

parseArrayIndex :: Parser Filter
parseArrayIndex = ArrayIndex <$> (string ".[" *> natural <* char ']')

parseArraySlice = undefined

parseValueIterator = undefined

parseOptional = undefined

parseComma = undefined


parseFilter :: Parser Filter
parseFilter = do
    f1 <- parseFilters
    f2 <- (symbol "|" *> parseFilter) <|> parseFilter <|> pure Identity
    return (Pipe f1 f2)
  where
    parseFilters = parseArrayIndex <|> parseObjectIndex <|> parseIdentity
  

parseConfig :: [String] -> Either String Config
parseConfig s = case s of
  [] -> Left "No filters provided"
  h : _ ->
    case parse parseFilter h of
      [(v, out)] -> case out of
        [] -> Right . ConfigC $ v
        _ -> Left $ "Compilation error, leftover: " ++ out
      e -> Left $ "Compilation error: " ++ show e
