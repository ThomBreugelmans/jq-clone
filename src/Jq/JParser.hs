module Jq.JParser where

import Parsing.Parsing
import Jq.Json
import Data.Char (readLitChar, chr, isHexDigit)
import Numeric (readHex)

parseJNull :: Parser JSON
parseJNull = do _ <- string "null"
                return JNull

parseJBool :: Parser JSON
parseJBool = do _ <- string "true"
                return (JBool True)
             <|> do _ <- string "false"
                    return (JBool False)

parseJNum :: Parser JSON
parseJNum = do (JNum <$> integer)
parseJFloat :: Parser JSON 
parseJFloat = do (JFloat <$> float)

-- TODO: support for escape chars
escapeUnicode :: Parser Char
escapeUnicode = chr . fst . head . readHex <$> sequenceA (replicate 4 (sat isHexDigit))
escape :: Parser Char
escape =  ('"' <$ string "\\\"") <|>
          ('\\' <$ string "\\\\") <|>
          ('/' <$ string "\\/") <|>
          ('\b' <$ string "\\b") <|>
          ('\f' <$ string "\\f") <|>
          ('\n' <$ string "\\n") <|>
          ('\r' <$ string "\\r") <|>
          ('\t' <$ string "\\t") <|>
          (string "\\u" *> escapeUnicode)
normal :: Parser Char
normal = sat ((&&) <$> (/= '"') <*> (/= '\\'))
parseJString :: Parser JSON
parseJString = do str <- char '"' *> many (normal <|> escape) <* char '"'
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
parseJSON = token $ parseJNull <|> parseJBool <|> parseJFloat <|> parseJNum <|> parseJString <|> parseJArray <|> parseJObject
