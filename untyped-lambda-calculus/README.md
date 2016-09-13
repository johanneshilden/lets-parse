
Untyped Lambda Calculus parser and interpreter.

In the untyped lambda calculus, a *term* is one of three things. Let T denote the set of terms, and let X be an infinite set of variables {x, y, z, ...}. Then,

* A variable is a term; x ∈ X ⇒ x ∈ T;
* Application of two terms is a term; M, N ∈ T ⇒ (M N) ∈ T; and
* A lambda abstraction is a term; x ∈ X Λ M ∈ T ⇒ (λx.M) ∈ T.

Nothing else is a term. Application is left-associative, so the term (s t u) is equivalent to ((s t) u).

Here is how one typically represents lambda terms in Haskell:

```haskell
data Term =
    Var !Text                -- Variable
  | App !Term !Term          -- Application
  | Lam !Text !Term          -- Lambda abstraction
  deriving (Show, Eq)
```

This is what we will use as the underlying type for our parser. 

```haskell
term :: Parser Term
```

Some famous expressions in the lambda calculus, represented using this data type, are 

* the identity function (λx.x);
```haskell
Lam "x" (Var "x")
```
* the Y-combinator (λf.(λx.(f (x x))) (λx.(f (x x)))); 
```haskell
Lam "f" (App (Lam "x" (App (Var "f") (App (Var "x") (Var "x")))) 
             (Lam "x" (App (Var "f") (App (Var "x") (Var "x")))))
```
* and the Church numerals (λf.λx.f x), (λf.λx.f (f x)), ...
```haskell
[ Lam "f" (Lam "x" (App (Var "f") (Var "x")))
, Lam "f" (Lam "x" (App (Var "f") (App (Var "f") (Var "x"))))
, ... ]
```

Later we will look at translation of these values into other, intermediate representation forms, more suitable for evaluation.

### Helpers

```haskell
oneOf :: String -> Parser Char
oneOf = satisfy . inClass

alphaNum :: Parser Char
alphaNum = satisfy isAlphaNum 

parens :: Parser a -> Parser a
parens p = char '(' *> p <* char ')'
```

### Lambda abstractions

```haskell
lambda :: Parser PTerm -> Parser PTerm
lambda body = do
    oneOf "\\λ"
    name <- many1 alphaNum
    skipSpace *> symbol <* skipSpace 
    body <- body
    return $ Lam (T.pack name) body
  where
    symbol = void (char '.') 
         <|> void (string "->")
```

### Variables

```haskell
var :: Parser PTerm
var = do
    name <- many1 alphaNum
    return $ Var (T.pack name)
```

### Applications

<!-- https://en.wikipedia.org/wiki/De_Bruijn_index -->
