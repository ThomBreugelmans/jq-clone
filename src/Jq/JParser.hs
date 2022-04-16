module Jq.JParser where

import Parsing.Parsing
import Jq.Json

parseJNull :: Parser JSON
parseJNull = do _ <- string "null"
                return JNull

parseJBool :: Parser JSON
parseJBool = do _ <- string "true"
                return (JBool True)
             <|> do _ <- string "false"
                    return (JBool False)

parseJNum :: Parser JSON
parseJNum = do JNum <$> int

-- TODO: support for escape chars
parseJString :: Parser JSON
parseJString = do str <- char '"' *> many (sat (/= '"')) <* char '"'
                  return (JString str)

seperateBy :: Parser a  -> Parser b -> Parser [b]
seperateBy sep element = (:) <$> element <*> many (sep *> element)
  <|> pure []

parseJArray :: Parser JSON
parseJArray = do xs <- char '[' *> space *> elements <* space <* char ']'
                 return (JArray xs)
  where
    elements = seperateBy (space *> char ',' <* space) parseJSON
    
parseJObject :: Parser JSON
parseJObject = JObject <$> (char '{' *> space *> keyvalues <* space <* char '}')
  where 
    keyvalues = seperateBy (space *> char ',' <* space) parseJObjKeyVal 

parseJObjKeyVal :: Parser (String, JSON)
parseJObjKeyVal = do k <- char '"' *> many (sat (/= '"')) <* char '"'
                     _ <- space *> char ':' <* space
                     v <- parseJSON
                     return (k, v)

parseJSON :: Parser JSON
parseJSON = token $ (parseJNull <|> parseJNum <|> parseJBool <|> parseJString <|> parseJArray <|> parseJObject)
