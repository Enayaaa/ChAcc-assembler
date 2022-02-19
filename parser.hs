{-# LANGUAGE LambdaCase #-}
module Parser where
import Control.Monad

-- | empty list of of results denotes failure
newtype Parser a = Parser {
                     -- | 
                     parse :: String -> [(a, String)] }


item :: Parser Char
item = Parser (\case
                  ""     -> []
                  (c:cs) -> [(c,cs)])

instance Functor Parser where
    fmap = liftM

instance Applicative Parser where
    pure = return
    (<*>) = ap

instance Monad Parser where
    return a = Parser (\cs -> [(a,cs)])
    p >>= f  = Parser (\cs -> concat [parse (f a) cs' | (a,cs') <- parse p cs])

-- | consume three chars, throw away 2nd and return tuple of 1st and 3rd char
p :: Parser (Char,Char)
p  = do { c <- item; item; d <- item; return (c,d); }
