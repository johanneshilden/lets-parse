This library is an example of how combinator parsing works in Haskell. From the [Wikipedia page](https://en.wikipedia.org/wiki/Parser_combinator) on parser combinators:

> In functional programming, parser combinators can be used to combine basic parsers to construct parsers for more complex rules.

This code is for demonstration purposes only. Don't use this parser in a real application. You may instead want to have a look at [Aeson](https://hackage.haskell.org/package/aeson) for a mature, efficient and well-maintained JSON parsing library.

### Dependencies

```
  build-depends:       base >=4.6, attoparsec >=0.13, text, containers
```

First, we need some imports.

```haskell
import Data.Attoparsec.Text
import Data.Text

import qualified Data.Map.Strict as H
```

The following language extensions are used:

```
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
```

### JSON

Looking at [json.org](http://json.org/), we can see that a JSON value is exactly one of the following; a *string*, a *number*, an *object*, an *array*, *true*, *false*, or *null*.

![value](value.gif)

> Image from [json.org](http://json.org/).

Based on this knowledge, we introduce a simple algebraic data type to represent JSON data in Haskell-land. 

```haskell
data Json = Object  !Dictionary  
          | Array   ![Json]  
          | Number  !Double 
          | String  !Text        
          | Boolean !Bool  
          | Null
    deriving (Show, Eq)
```

Most of this is straightforward. Each data constructor represents a track in the above diagram, except for Boolean, since it makes sense to combine *true* and *false* into a single constructor that accepts a native `Bool` as its argument. `Dictionary` is a type synonym for a `Map` with `Text` keys and JSON value entries, defined as

```haskell
type Dictionary = H.Map Text Json
```

We will need [combinators](https://en.wikipedia.org/wiki/Combinatory_logic#Combinatory_terms) to parse each of these separately.

```haskell
jsonString, jsonNumber, jsonBoolean, jsonNull, jsonObject, jsonArray, jsonValue :: Parser Json
```

The top-level value parser is the easiest to implement since it, by definition, should match exactly one of the other types.

```haskell
jsonValue = jsonObject
        <|> jsonArray
        <|> jsonNumber
        <|> jsonString
        <|> jsonBoolean
        <|> jsonNull
```

Unsurprisingly, this type looks very similar to the railroad diagram above from the JSON specification.

### Whitespace

> Whitespace can be inserted between any pair of tokens. 

We want to allow whitespace characters inserted before the actual value being parsed. I am therefore going to wrap the `jsonValue` parser in another function which will serve as the main API for the library.

```haskell
-- | Decode JSON data, ignoring leading blank space.
json :: Parser Json
json = skipSpace *> jsonValue 
```

It is also useful to introduce a simple helper that translates a parser to one which ignores whitespace characters on both sides of the input.

```haskell
padded :: Parser a -> Parser a
padded parser = skipSpace *> parser <* skipSpace
```

### Some more helpers

We also define some helpers that will be used later.

```haskell
oneOf :: String -> Parser Char
oneOf = satisfy . inClass
```

This parser will accept any character in the provided `String`.

```haskell
maybeOption :: Parser a -> Parser (Maybe a)
maybeOption p = option Nothing (Just <$> p)
```

This function returns the result of running some action, wrapped in a `Maybe` type, i.e., `Nothing` if the parser fails without consuming input, and otherwise `Just` the result.

### String

![string](string.gif)

> Image from [json.org](http://json.org/).

A string literal is a (possibly empty) sequence of Unicode characters of valid type, enclosed in quotes. (We are not going to worry about what *valid character* means right now.) To make things easier, we can define a function which will behave similar to `manyTill`, except that it is necessary to satisfy the provided parser both at the beginning and at the end of the input.

> `manyTill p` end applies action `p` zero or more times until action end succeeds, and returns the list of values returned by `p`.

```haskell
manyEnclosedIn :: Parser a -> Parser b -> Parser [a] 
manyEnclosedIn parser encl = encl *> manyTill parser encl
```

We can now define the string parser in terms of `manyEnclosedIn`.

```haskell
-- | Parse a string literal, i.e., zero or more characters enclosed in double quotes.
literal :: Parser Text
literal = pack <$> validChar `manyEnclosedIn` char '"' 
  where
    ... 
```

Now going back to the characters accepted by `validChar`. Looking at the specification, we see that a *valid character* here is one of 

1. `\` followed by any of `"`, `\`, `/`, `b`, `f`, `n`, `r`, or `t`;
2. a Unicode escape sequence; or
3. any character except `\` and `"`.

So, something like

```haskell
    validChar = special 
            <|> unicode 
            <|> notChar '\\'
```

should do the trick. Note that we do not have to exclude `"` from the characters accepted by the last option since `manyTill` will terminate when it runs into this character anyway.

```haskell
    special = char '\\' *> oneOf "\"\\/bfnrt"
```

```haskell
    unicode :: Parser Char
    unicode = do
        string "\\u"
        code <- count 4 hexDigit
        return $ chr $ read $ "0x" ++ code
```

```haskell
    hexDigit = oneOf "0123456789abcdefABCDEF"
```

Having this, parsing a JSON string is now as easy as lifting the `String` constructor into the `Parser` monad.

```haskell
-- | Decode a JSON string literal.
jsonString :: Parser Json
jsonString = String <$> literal 
```

```haskell
λ> :t String
String :: Text -> Json
λ> :t literal
literal :: Parser Text
λ> :t fmap String literal      -- == String <$> literal
fmap String literal :: Parser Json
```

### Number

Numbers are slightly trickier. In particular, we need to consider scientific notation (standard form) and make sure that a number does not start with a zero unless it has a decimal point or is exactly 0 (i.e., things like 0003 shouldn't parse). Octal and hexadecimal numbers are not supported in JSON.

![number](number.gif)

> Image from [json.org](http://json.org/).

Conceptually, we can divide the number parser up into four parts:

```
[-] (integral part) [fractional part] [exponential part]
```

Everything is optional, except the integer component.

```haskell
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
    ...
```

### Boolean

```haskell
-- | Decode a boolean.
jsonBoolean :: Parser Json
jsonBoolean = true <|> false
  where
    true  = "true"  *> return (Boolean True)
    false = "false" *> return (Boolean False)
```

### Null

```haskell
-- | Decode a null value.
jsonNull :: Parser Json
jsonNull = "null" *> return Null
```

### Object

![object](object.gif)

> Image from [json.org](http://json.org/).

```haskell
keyValuePair :: Parser (Text, Json)
keyValuePair = do
    key <- literal
    padded (char ':')             -- Ignore whitespace before and after the colon
    value <- jsonValue
    return (key, value)
```

```haskell
jsonObject :: Parser Json
jsonObject = do
    char '{'
    body <- padded keyValuePair `sepBy` char ','
    char '}'
    return $ Object $ H.fromList body
  where
    keyValuePair = ...            -- See above
```

```haskell
-- | Decode a JSON object.
jsonObject :: Parser Json
jsonObject = char '{' *> pairs <* char '}'
  where
    pairs = Object . H.fromList <$> padded keyValuePair `sepBy` char ','
    keyValuePair = do
        key <- literal
        padded (char ':') 
        value <- jsonValue
        return (key, value)    
```

### Array

![array](array.gif)

> Image from [json.org](http://json.org/).

Using `do`-notation, this could look something like the following.

```haskell
-- | Decode a JSON array.
jsonArray :: Parser Json
jsonArray = do
    char '['
    values <- value `sepBy` char ','
    char ']'
    return $ Array values
  where
    value = padded jsonValue
```

```haskell
jsonArray = let values = padded jsonValue `sepBy` char ',' 
             in char '[' *> (Array <$> values) <* char ']'
```
