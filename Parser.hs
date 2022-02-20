{-# LANGUAGE LambdaCase #-}
module Parser (module Parser, module Control.Applicative) where

import Control.Monad
import Control.Applicative
import Data.Char

-- | A parser for things is a function from strings to list of pairs of things and strings.
-- The empty list of of results denotes failure of parsing.
newtype Parser a = Parser (String -> [(a, String)])

parse (Parser p) = p

item :: Parser Char
item = Parser (\case
                  ""     -> []
                  (c:cs) -> [(c,cs)])

instance Functor Parser where
    fmap = liftM
    -- fmap g p = P (\inp -> case parse p inp of
                            --[]        -> []
                            --[(v,out)] -> [(g v, out)])

instance Applicative Parser where
    pure a = Parser (\cs -> [(a,cs)])
    (<*>) = ap

instance Monad Parser where
    p >>= f  = Parser (\inp -> concat [parse (f a) out | (a,out) <- parse p inp])

-- ======================= Choice combinators ======================= 

instance Alternative Parser where
  empty = Parser (const [])
  p <|> q = Parser (\cs -> case parse p cs of 
                             [] -> parse q cs
                             res -> res)

-- | Keep only one first parsing successfully result,
-- throw away other ways the parsing could succeed when making a choice
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser (\cs -> case parse (p <|> q) cs of
                           []     -> []
                           (x:xs) -> [x])

sat :: (Char -> Bool) -> Parser Char
sat p = do c <- item
           guard (p c) -- fail if predicate doesn't hold
           return c

char :: Char -> Parser Char
char c = sat (c ==)

-- ======================= Recursion combinators =======================

-- | Parse a specific string
string :: String -> Parser String
string "" = return ""
string (c:cs) = do { char c; string cs; return (c:cs); }

-- | Parse repeated applications of a parser p;
-- the many combinator permits zero or more applications of p
-- `many`

-- | Parse repeated applications of a parser p;
-- the some combinator permits one or more applications of p
-- `some`

-- | Parse repeated applications of a parser p, separated by applications
-- of a parser sep whose result values are thrown away. Permits zero applications
sepBy :: Parser a -> Parser a -> Parser [a]
p `sepBy` sep = (p `sepBy1` sep) +++ return []

-- | Parse repeated applications of a parser p, separated by applications
-- of a parser sep whose result values are thrown away. Permits one or more applications
sepBy1 :: Parser a -> Parser a -> Parser [a]
p `sepBy1` sep = do a <- p
                    as <- many (do {sep; p})
                    return (a:as)

-- | Parse repeated applications of a parser p, separated by applications of a parser
-- op whose result value is an operator that is assumed to associate to the left,
-- and which is used to combine the results from the p parsers
-- Can be applied zero or more times.
chainl :: Parser a -> Parser (a->a->a) -> a -> Parser a
chainl p op a = (p `chainl1` op) +++ return a

-- | Parse repeated applications of a parser p, separated by applications of a parser
-- op whose result value is an operator that is assumed to associate to the left,
-- and which is used to combine the results from the p parsers.
-- Can be applied one or more times.
chainl1 :: Parser a -> Parser (a->a->a) -> Parser a
p `chainl1` op = do { a <- p; rest a}
                    where
                        rest a = do f <- op
                                    b <- p
                                    rest (f a b)
                                +++ return a

-- chainr  and chainr1 can be implemented simmiliarly


-- ======================= Lexical combinators =======================

digit :: Parser Int
digit = do x <- digitChar
           return (ord x - ord '0')

letter :: Parser Char
letter = sat isLetter

digitChar :: Parser Char
digitChar = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

alphanum :: Parser Char
alphanum = sat isAlphaNum

nat :: Parser Int
nat = do x <- some digitChar
         return (read x)

int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
       <|> nat

-- | Parse a string of spaces, tabs
space :: Parser String
space = many (sat (\c -> c == '\t' || isSeparator c))

-- | Parse an identifier, starting with a lowercase and zero or more alphanumerics.
identifier :: Parser String
identifier = do x <- lower
                xs <- many alphanum
                return (x:xs)

-- | Parse a token using a parser p, throwing away any trailing space
token :: Parser a -> Parser a
token p = do a <- p
             space
             return a

-- | Parse a symbolic token
symbol :: String -> Parser String
symbol cs = token (string cs)

-- | Apply a parser p, throwing away any leading space
apply :: Parser a -> String -> [(a,String)]
apply p = parse $ do { space; p }

-- ======================= Example =======================
{-
expr :: Parser Int
addop :: Parser (Int -> Int -> Int)
mulop :: Parser (Int -> Int -> Int)

expr = term `chainl1` addop
term = factor `chainl1` mulop
factor = digit +++ do { symbol "("; n <- expr; symbol ")"; return n }

addop = do { symbol "+"; return (+) } +++ do { symbol "-"; return (-) }
mulop = do { symbol "*"; return (*) } +++ do { symbol "/"; return div }

res :: Int
res = fst . head $ apply expr " (1 - 2*3)+4 " -- -1

-}
