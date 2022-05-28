module Jq.CParser where

import Parsing.Parsing
import Jq.Filters

parseIdentity :: Parser Filter
parseIdentity = do
  _ <- space *> char '.' <* space
  return Identity

parseGroup :: Parser Filter
parseGroup = do
  filts <- symbol "(" *> parseFilter <* symbol ")"
  return (Group [filts])

parseObjectIndex :: Parser Filter
parseObjectIndex = ObjectIndex <$> (string ".[" *> space *> char '"' *> many (sat (/= '"')) <* char '"' <* space <* char ']')
  <|>
    ObjectIndex <$> (char '.' *> ident)
  <|>
    ObjectIndex <$> (string ".\"" *> ident <* char '"')

parseArrayIndex :: Parser Filter
parseArrayIndex = ArrayIndex <$> (string ".[" *> natural <* char ']')

parseArraySlice :: Parser Filter
parseArraySlice = do
  start <- string ".[" *> natural <* char ':'
  end <- natural <|> pure (-1)
  _ <- symbol "]"
  return (ArraySlice start end)

parseValueIterator :: Parser Filter
parseValueIterator = do xs <- string ".[" *> space *> elements <* space <* char ']'
                        return (ValueIterator xs)
  where
    elements = seperateBy (space *> char ',' <* space) natural
    seperateBy sep element = (:) <$> element <*> many (sep *> element)
      <|> pure []

parseOptional :: Parser Filter
parseOptional = do
  filt <- (parseObjectIndex <|> parseArraySlice <|> parseArrayIndex <|> parseValueIterator) <* symbol "?"
  return (Optional filt)
  
parseAllIterator :: Parser Filter 
parseAllIterator = do 
  _ <- symbol "." *> symbol "[" *> symbol "]"
  return Values

parseComma :: Parser Filter
parseComma = do
  f1 <- parseUnaryFilters
  f2 <- symbol "," *> parseUnaryFilters
  return (Comma f1 f2)
  
parsePipe :: Parser Filter
parsePipe = do 
  f1 <- parseComma <|> parseUnaryFilters
  f2 <- symbol "|" *> parseFilter
  return (Pipe f1 f2)

parseUnaryFilters :: Parser Filter 
parseUnaryFilters = parseGroup <|> parseOptional <|> parseAllIterator <|> parseArraySlice <|> parseArrayIndex <|> parseObjectIndex <|> parseValueIterator <|> parseIdentity

parseFilter :: Parser Filter
parseFilter = do
    f1 <- parsePipe <|> parseComma <|> parseUnaryFilters
    f2 <- parseFilter <|> pure Identity
    return (Pipe f1 f2)


parseConfig :: [String] -> Either String Config
parseConfig s = case s of
  [] -> Left "No filters provided"
  h : _ ->
    case parse parseFilter h of
      [(v, out)] -> case out of
        [] -> Right . ConfigC $ v
        _ -> Left $ "Compilation error, leftover: " ++ out
      e -> Left $ "Compilation error: " ++ show e
