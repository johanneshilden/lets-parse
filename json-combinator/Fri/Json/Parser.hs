{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
module Fri.Json.Parser where

import Control.Applicative        ( (<$>), (<*), (*>), (<|>) )
import Data.Monoid
import Data.Attoparsec.Combinator ( lookAhead )
import Data.Attoparsec.Text
import Data.Char                  ( chr ) 
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

-- | Parse a string literal, i.e., zero or more characters enclosed in 
--   double quotes.
literal :: Parser Text
literal = pack <$> chars
  where
    chars = char '"' *> manyTill validChar (char '"')
    validChar = special 
            <|> unicode 
            <|> notChar '\\'
    -- Escaped special character
    special = char '\\' *> oneOf "\"\\/bfnrt"
    -- Unicode escape sequence
    unicode :: Parser Char
    unicode = do
        string "\\u"
        code <- count 4 hexDigit
        return $ chr $ read $ "0x" ++ code
    -- A single hexadecimal digit
    hexDigit = oneOf "0123456789abcdefABCDEF"

-- | Decode a JSON string literal.
jsonString :: Parser Json
jsonString = String <$> literal 

-- | Decode a JSON number.
jsonNumber :: Parser Json
jsonNumber = do
    sign <- maybeOption (char '-')
    int  <- return <$> char '0' <|> many1 digit
    frac <- fractional
    pow  <- option 0 exponent
    let digits = read (int <> frac) * 10 ^ pow 
    return $ Number $ case sign of
      Nothing -> digits
      Just _  -> negate digits
  where
    -- Fractional part
    fractional = maybeOption (char '.') 
        >>= \case 
          Nothing -> return []
          Just _  -> many1 digit >>= return <$> (:) '.'
    -- Exponent (scientific notation)
    exponent = do
        prefix <- oneOf "eE"
        sign   <- maybeOption (oneOf "+-")
        digits <- read <$> many1 digit
        return $ case sign of
          Just '-' -> negate digits
          _        -> digits

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
jsonArray = char '[' *> (Array <$> padded jsonValue `sepBy` char ',') <* char ']'

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
padded parser = skipSpace *> parser <* skipSpace

