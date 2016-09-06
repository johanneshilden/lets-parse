
Untyped Lambda Calculus parser and interpreter.

In the untyped lambda calculus, a *term* is one of three things. Let T denote the set of terms, and let X be an infinite set of variables {x, y, z, ...}. Then,

* A variable is a term; x ∈ X ⇒ x ∈ T;
* Application of two terms is a term; M, N ∈ T ⇒ (M N) ∈ T; and
* A lambda abstraction is a term; x ∈ X Λ M ∈ T ⇒ (λx.M) ∈ T.

Nothing else is a term.

Here is how we can represent a term in Haskell, using an algebraic data type:

```haskell
data Term =
    Var T.Text              -- Variable
  | App Term Term           -- Application
  | Lam T.Text Term         -- Lambda abstraction
  deriving (Show, Eq)
```

Some famous terms in the lambda calculus are 

* the identity function (λx.x);
* the Y-combinator (λf.(λx.(f (x x))) (λx.(f (x x)))); and
* the Church numerals (λf.λx.f x), (λf.λx.f (f x)), ...
