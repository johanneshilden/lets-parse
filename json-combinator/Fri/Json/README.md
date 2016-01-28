

```haskell
import Data.Text

data Json = Object  !Dictionary  
          | Array   ![Json]  
          | Number  !Double 
          | String  !Text        
          | Boolean !Bool  
          | Null
    deriving (Show, Eq)
```

```haskell
import qualified Data.Map.Strict as H

type Dictionary = H.Map Text Json
```

```haskell
jsonString :: Parser Json
jsonNumber :: Parser Json
jsonBoolean :: Parser Json
jsonNull :: Parser Json
jsonObject :: Parser Json
jsonArray :: Parser Json
jsonValue :: Parser Json
json :: Parser Json
```
