{-# LANGUAGE OverloadedStrings #-}
module Fri.Json.Parser where

import Control.Applicative        ( (<$>), (<*), (*>), (<|>) )
import Data.Attoparsec.Combinator ( lookAhead )
import Data.Attoparsec.Text
import Data.Text                  ( Text, pack, unpack )

import qualified Data.Map.Strict as H

type Dictionary = H.Map Text Json

-- | Data type to represent a JSON value.
data Json = Object !Dictionary  | Array ![Json]  | Number !Double 
          | String !Text        | Boolean !Bool  | Null
    deriving (Show, Eq)

-- | Parse a string literal, i.e., zero or more characters enclosed in 
--   double quotes.
literal :: Parser Text
{-# INLINE literal #-}
literal = pack <$> chars
  where
    chars = char '"' *> manyTill anyChar (char '"')

-- | Decode a JSON string literal.
jsonString :: Parser Json
jsonString = String <$> literal 

-- | Decode a JSON number.
jsonNumber :: Parser Json
jsonNumber = lookAhead (notChar '+') >> Number <$> double

-- | Decode a boolean.
jsonBoolean :: Parser Json
jsonBoolean = true <|> false
  where
    true  = "true"  *> return (Boolean True)
    false = "false" *> return (Boolean False)

-- | Decode a null value.
jsonNull :: Parser Json
jsonNull = "null" *> return Null

-- | Decode a JSON object.
jsonObject :: Parser Json
jsonObject = do
    char '{'
    body <- padded keyValuePair `sepBy` char ','
    char '}'
    return $ Object $ H.fromList body
  where
    keyValuePair :: Parser (Text, Json)
    keyValuePair = do
        key <- literal
        padded (char ':')
        value <- jsonValue
        return (key, value)

-- | Decode a JSON array.
jsonArray :: Parser Json
jsonArray = do
    char '['
    values <- padded jsonValue `sepBy` char ','
    char ']'
    return $ Array values

-- | Decode a JSON value.
jsonValue :: Parser Json
jsonValue = jsonObject
        <|> jsonArray
        <|> jsonNumber
        <|> jsonString
        <|> jsonBoolean
        <|> jsonNull

-- | Decode JSON data with possible leading blank space.
json :: Parser Json
json = skipSpace *> jsonValue 

padded :: Parser a -> Parser a
padded match = skipSpace *> match <* skipSpace
