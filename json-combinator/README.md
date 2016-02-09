This is a simple example of how combinator parsing works in Haskell. 

> Don't use this parser in production code. You may instead want to have a look at [Aeson](https://hackage.haskell.org/package/aeson) for a mature, efficient and well-maintained JSON parsing library.

```
  build-depends:       base >=4.6, attoparsec >=0.13, text, containers
```

First, we need some imports.

```haskell
import Data.Attoparsec.Text
import Data.Text

import qualified Data.Map.Strict as H
```

Let's introduce a simple algebraic data type to represent JSON data in Haskell-land. Looking at [json.org](http://json.org/), we can see that a JSON value is exactly one of the following; a *string*, a *number*, an *object*, an *array*, *true*, *false*, or *null*.

![value](value.gif)

```haskell
data Json = Object  !Dictionary  
          | Array   ![Json]  
          | Number  !Double 
          | String  !Text        
          | Boolean !Bool  
          | Null
    deriving (Show, Eq)
```

Most of this is straightforward. It makes sense to combine *true* and *false* into a single data constructor. `Dictionary` is a type synonym for a `Map` with `Text` keys and JSON value entries, defined as

```haskell
type Dictionary = H.Map Text Json
```

We will need combinators to parse each of these separately.

```haskell
jsonString, jsonNumber, jsonBoolean, jsonNull, jsonObject, jsonArray, jsonValue :: Parser Json
```

The top-level value parser is the easiest to implement since it, by definition, should match exactly on of the other types.

```haskell
jsonValue = jsonObject
        <|> jsonArray
        <|> jsonNumber
        <|> jsonString
        <|> jsonBoolean
        <|> jsonNull
```

```haskell
-- | Parse a string literal, i.e., zero or more characters enclosed in double quotes.
literal :: Parser Text
literal = ...
```

Parsing a JSON string is now as easy as lifting the String constructor into the Parser monad and ... 

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
