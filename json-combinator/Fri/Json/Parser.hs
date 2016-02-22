{-# LANGUAGE OverloadedStrings #-}
module Fri.Json.Parser where

import Control.Applicative        ( (<$>), (<*>), (<*), (*>), (<|>) )
import Data.Attoparsec.Text
import Data.Char                  ( chr ) 
import Data.Monoid                ( (<>) )
import Data.Text                  ( Text, pack, unpack )

import qualified Data.Map.Strict as H

type Dictionary = H.Map Text Json

-- | Data type to represent a JSON value.
data Json = Object !Dictionary  | Array ![Json]  | Number !Double 
          | String !Text        | Boolean !Bool  | Null
    deriving (Show, Eq)

--
-- Various helpers
--

oneOf :: String -> Parser Char
oneOf = satisfy . inClass

maybeOption :: Parser a -> Parser (Maybe a)
maybeOption p = option Nothing (Just <$> p)

has :: Parser a -> Parser Bool
has p = option False (const True <$> p)

manyEnclosedIn :: Parser a -> Parser b -> Parser [a] 
manyEnclosedIn parser fence = fence *> manyTill parser fence

padded :: Parser a -> Parser a
padded parser = skipSpace *> parser <* skipSpace

-- | Parse a string literal, i.e., zero or more characters enclosed in 
--   double quotes.
literal :: Parser Text
literal = pack <$> validChar `manyEnclosedIn` char '"' 
  where
    validChar = special 
            <|> unicode 
            <|> notChar '\\'
    -- Escaped special character
    special = char '\\' *> oneOf "\"\\/bfnrt"
    -- Unicode escape sequence
    unicode :: Parser Char
    unicode = do
        "\\u"
        code <- count 4 hexDigit
        return $ chr $ read $ "0x" ++ code
    -- A single hexadecimal digit
    hexDigit = oneOf "0123456789abcdefABCDEF"

jsonString, jsonNumber, jsonBoolean, jsonNull, jsonObject, jsonArray, json, jsonValue :: Parser Json

-- | Decode a JSON string literal.
jsonString = String <$> literal 

-- | Decode a JSON number.
jsonNumber = do
    negative <- has (char '-')
    int  <- unpack <$> "0" <|> many1 digit
    frac <- option "" fractional
    pow  <- option 0 exponent
    let number = read (int <> frac) * 10 ^ pow 
    return $ Number 
           $ if negative 
                then negate number 
                else number
  where
    -- Fractional part
    fractional = (:) <$> char '.' <*> many1 digit
    -- Exponent (scientific notation)
    exponent = do
        prefix <- oneOf "eE"
        sign   <- maybeOption (oneOf "+-")
        digits <- read <$> many1 digit
        return $ case sign of
          Just '-' -> negate digits
          _        -> digits

-- | Decode a boolean.
jsonBoolean = true <|> false
  where
    true  = "true"  *> return (Boolean True)
    false = "false" *> return (Boolean False)

-- | Decode a null value.
jsonNull = "null" *> return Null

-- | Decode a JSON object.
jsonObject = char '{' *> pairs <* char '}'
  where
    pairs = Object . H.fromList <$> padded keyValuePair `sepBy` char ','
    keyValuePair :: Parser (Text, Json)
    keyValuePair = do
        key <- literal
        padded (char ':')
        value <- jsonValue
        return (key, value)

-- | Decode a JSON array.
jsonArray = let values = padded jsonValue `sepBy` char ',' 
             in char '[' *> (Array <$> values) <* char ']'

-- | Decode a JSON value.
jsonValue = jsonObject
        <|> jsonArray
        <|> jsonNumber
        <|> jsonString
        <|> jsonBoolean
        <|> jsonNull

-- | Decode JSON data with possible leading blank space.
json = skipSpace *> jsonValue 

