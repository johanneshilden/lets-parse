
```haskell
data PTerm =
    PLam T.Text PTerm        -- Lambda abstraction
  | PVar T.Text              -- Variable
  | PApp PTerm PTerm         -- Application
  deriving (Show, Eq)
```
