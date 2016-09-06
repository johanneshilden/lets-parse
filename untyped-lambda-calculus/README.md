
Untyped Lambda Calculus parser and interpreter.

In the untyped λ calculus, a term is one of three things:

```
x
M N
\x.M
```

```haskell
data PTerm =
    PLam T.Text PTerm        -- Lambda abstraction
  | PVar T.Text              -- Variable
  | PApp PTerm PTerm         -- Application
  deriving (Show, Eq)
```
