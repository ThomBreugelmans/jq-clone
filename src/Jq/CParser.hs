module Jq.CParser where

import Parsing.Parsing
import Jq.Filters
import Data.Char (chr, isHexDigit)
import Numeric (readHex)

parseIdentity :: Parser Filter
parseIdentity = do
  _ <- space *> char '.' <* space
  return Identity

parseGroup :: Parser Filter
parseGroup = do
  filts <- symbol "(" *> parseFilter <* symbol ")"
  return (Group [filts])

parseObjectIndex :: Parser Filter
parseObjectIndex = ObjectIndex <$> (string ".[" *> space *> char '"' *> (concat <$> many (normal <|> escape)) <* char '"' <* space <* char ']')
  <|>
    ObjectIndex <$> (char '.' *> ident)
  <|>
    ObjectIndex <$> (string ".\"" *> (concat <$> many (normal <|> escape)) <* char '"')
  where
    escape =  (string "\\u" *> ((: []) <$> (chr . fst . head . readHex <$> sequenceA (replicate 4 (sat isHexDigit))))) <|>
              (string "\\\\") <|>
              (string "\\n") <|>
              (string "\\t") <|>
              (string "\\r") <|>
              (string "\\f") <|>
              (string "\\b") <|>
              (string "\\/" *> pure "/") <|>
              (string "\\\"")
    normal = (: []) <$> sat ((&&) <$> (/= '"') <*> (/= '\\'))

parseArrayIndex :: Parser Filter
parseArrayIndex = ArrayIndex <$> (string ".[" *> int <* char ']')

parseArraySlice :: Parser Filter
parseArraySlice = do
  _ <- string ".["
  start <- space *> integer <|> pure 0
  _ <- space *> char ':'
  end <- space *> integer <|> pure (maxBound :: Int)
  _ <- space *> symbol "]"
  return (ArraySlice start end)

parseArrayValueIterator :: Parser Filter
parseArrayValueIterator = do
  xs <- string ".[" *> space *> elements <* space <* char ']'
  return (ArrayValueIterator xs)
  where
    elements = seperateBy (space *> char ',' <* space) natural
    seperateBy sep element = (:) <$> element <*> many (sep *> element)
      <|> pure []
parseObjectValueIterator :: Parser Filter
parseObjectValueIterator = do
  xs <- string ".[" *> space *> elements <* space <* char ']'
  return (ObjectValueIterator (concat xs))
  where
    elements = seperateBy (space *> char ',' <* space) (char '"' *> many (normal <|> escape) <* char '"')
    seperateBy sep element = (:) <$> element <*> many (sep *> element)
      <|> pure []
    escape =  (string "\\u" *> ((: []) <$> (chr . fst . head . readHex <$> sequenceA (replicate 4 (sat isHexDigit))))) <|>
              (string "\\\\") <|>
              (string "\\n") <|>
              (string "\\t") <|>
              (string "\\r") <|>
              (string "\\f") <|>
              (string "\\b") <|>
              (string "\\/" *> pure "/") <|>
              (string "\\\"")
    normal = (: []) <$> sat ((&&) <$> (/= '"') <*> (/= '\\'))

parseOptional :: Parser Filter
parseOptional = do
  filt <- parseIndexers <* symbol "?"
  return (Optional filt)
  
--parseAllIterator :: Parser Filter
--parseAllIterator = do
--  _ <- symbol "." *> symbol "[" *> symbol "]"
--  return Values

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
parseUnaryFilters = parseGroup <|>
                    parseOptional <|>
                    parseIndexers <|>
                    parseIdentity

parseIndexers :: Parser Filter
parseIndexers = parseArrayIndex <|>
                parseObjectIndex <|>
                parseArraySlice <|>
                parseArrayValueIterator <|> 
                parseObjectValueIterator

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
