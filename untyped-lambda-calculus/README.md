
Untyped Lambda Calculus parser and interpreter.

In the untyped λ calculus, a term is one of three things:

Let Λ denote the set of terms, and V the set of variables.

* A variable (x) is a term; x ∈ V ⇒ x ∈ ∈
* Application of two terms (M N) is a term;
* A lambda abstraction is a term (λx.M).

```haskell
data PTerm =
    PLam T.Text PTerm        -- Lambda abstraction
  | PVar T.Text              -- Variable
  | PApp PTerm PTerm         -- Application
  deriving (Show, Eq)
```
