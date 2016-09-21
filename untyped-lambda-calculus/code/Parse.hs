module Parse ( term ) where

import Data.Attoparsec.Text
import Data.Char                      ( isAlphaNum, isAscii ) 
import Control.Applicative            ( (<$>), (<*>), (<*), (*>), (<|>) )
import Data.Text                      ( Text, pack )

-- | Lambda expression data type representation for parsing
data Term =
    Var !Text                -- ^ Variable
  | App !Term !Term          -- ^ Application
  | Lam !Text !Term          -- ^ Lambda abstraction
  deriving (Show, Eq)

-- Helpers --------------------------------------------------------------------

oneOf :: String -> Parser Char
oneOf = satisfy . inClass

alphaNum :: Parser Char
alphaNum = satisfy valid
  where
    valid c = all ($ c) [ isAscii, isAlphaNum ]

parens :: Parser a -> Parser a
parens p = char '(' *> p <* char ')'

-------------------------------------------------------------------------------

-- | Lambda abstraction parser
lambda :: Parser Term -> Parser Term
lambda term = do
    oneOf "\955\\"                    -- Lambda or backslash 
    name <- many1 alphaNum
    skipSpace *> char '.' <* skipSpace 
    body <- term
    return $ Lam (pack name) body

-- | Variable parser
var :: Parser Term
var = do
    name <- many1 alphaNum
    return $ Var (pack name)

-- | Term parser
term :: Parser Term
term = do
    terms <- many1 (skipSpace >> expr)
    return $ foldl1 App terms
  where
    expr :: Parser Term
    expr = var            -- x
       <|> lambda term    -- \x.M
       <|> parens term    -- (M)

