module Jq.JParser where

import Parsing.Parsing
import Jq.Json
import Data.Char (readLitChar, chr, isHexDigit)
import Numeric (readHex)
import Data.Map (fromList)

parseJNull :: Parser JSON
parseJNull = do _ <- string "null"
                return JNull

parseJBool :: Parser JSON
parseJBool = do _ <- string "true"
                return (JBool True)
             <|> do _ <- string "false"
                    return (JBool False)

parseJNum :: Parser JSON
parseJNum = do JNum <$> integer
parseJFloat :: Parser JSON 
parseJFloat = do JFloat <$> float

escape :: Parser String
escape =  (string "\\u" *> ((: []) <$> (chr . fst . head . readHex <$> sequenceA (replicate 4 (sat isHexDigit))))) <|>
          (string "\\\\") <|>
          (string "\\n") <|>
          (string "\\t") <|>
          (string "\\r") <|>
          (string "\\f") <|>
          (string "\\b") <|>
          (string "\\/" *> pure "/") <|>
          (string "\\\"")
normal :: Parser String
normal = (: []) <$> sat ((&&) <$> (/= '"') <*> (/= '\\'))
parseJString :: Parser JSON
parseJString = do str <- char '"' *> many (normal <|> escape) <* char '"'
                  return (JString (concat str))

seperateBy :: Parser a  -> Parser b -> Parser [b]
seperateBy sep element = (:) <$> element <*> many (sep *> element)
  <|> pure []

parseJArray :: Parser JSON
parseJArray = 
  do _ <- char '[' *> space <* char ']'
     return (JArray [])
  <|>
  do xs <- char '[' *> space *> elements <* space <* char ']'
     return (JArray xs)
  where
    elements = seperateBy (space *> char ',' <* space) parseJSON

parseJObject :: Parser JSON
parseJObject = JObject . fromList <$> (char '{' *> space *> keyvalues <* space <* char '}')
  where
    keyvalues = seperateBy (space *> char ',' <* space) parseJObjKeyVal

parseJObjKeyVal :: Parser (String, JSON)
parseJObjKeyVal = do k <- char '"' *> many (normal <|> escape) <* char '"'
                     _ <- space *> char ':' <* space
                     v <- parseJSON
                     return (concat k, v)

parseJSON :: Parser JSON
parseJSON = token $ parseJNull <|> parseJArray <|> parseJObject <|> parseJBool <|> parseJString <|> parseJFloat
