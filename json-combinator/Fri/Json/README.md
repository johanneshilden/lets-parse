

```haskell
import Data.Attoparsec.Text
import Data.Text

import qualified Data.Map.Strict as H
```

```haskell
data Json = Object  !Dictionary  
          | Array   ![Json]  
          | Number  !Double 
          | String  !Text        
          | Boolean !Bool  
          | Null
    deriving (Show, Eq)
```

```haskell
type Dictionary = H.Map Text Json
```

```haskell
jsonString, jsonNumber, jsonBoolean, jsonNull, jsonObject, jsonArray, jsonValue, json :: Parser Json
```
