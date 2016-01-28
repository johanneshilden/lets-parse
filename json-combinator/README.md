This is an example of how parser combinators work in Haskell. Don't use this parser in production code.

```
  build-depends:       base >=4.6, attoparsec >=0.13, text, containers
```

```haskell
import Data.Attoparsec.Text
import Data.Text

import qualified Data.Map.Strict as H
```
First, we'll introduce a simple algebraic data type to represent a JSON value in Haskell-land.

```haskell
data Json = Object  !Dictionary  
          | Array   ![Json]  
          | Number  !Double 
          | String  !Text        
          | Boolean !Bool  
          | Null
    deriving (Show, Eq)
```

`Dictionary` is a type synonym for a `Map` with Text keys and Json value entries:

```haskell
type Dictionary = H.Map Text Json
```

```haskell
jsonString, jsonNumber, jsonBoolean, jsonNull, jsonObject, jsonArray, jsonValue, json :: Parser Json
```

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
