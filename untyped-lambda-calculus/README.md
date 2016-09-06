
Untyped Lambda Calculus parser and interpreter.

In the untyped λ calculus, a term is one of three things:

* A variable (x) is a term;
* Application of two terms (M N) is a term;
* A lambda abstraction is a term (\x.M).

```haskell
data PTerm =
    PLam T.Text PTerm        -- Lambda abstraction
  | PVar T.Text              -- Variable
  | PApp PTerm PTerm         -- Application
  deriving (Show, Eq)
```
