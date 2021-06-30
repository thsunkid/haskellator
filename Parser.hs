module Parser where

import Control.Applicative
import Data.Char


newtype Parser a = P (String -> [(a,String)])

parse :: Parser a -> String -> [(a,String)]
parse (P p) str = p str

instance Functor Parser where
    -- fmap :: (a->b) -> Parser a -> Parser b
    fmap f p = P (\inp -> case parse p inp of
                            [] -> []
                            [(x,xs)]->[(f x, xs)])
        
instance Applicative Parser where
    -- pure :: a -> Parser a
    pure a = P (\inp -> [(a,inp)])

    -- <*> :: Parser (a->b) -> Parser a -> Parser b
    pf <*> px = P (\inp -> case parse pf inp of
                             []        -> []
                             [(f,out)] -> parse (fmap f px) out)

instance Monad Parser where
    return = pure

    px >>= f = P(\inp -> case parse px inp of
                           []        -> []
                           [(x,out)] -> parse (f x) out)


instance Alternative Parser where
    empty = P(\inp-> [])

    -- <|> :: Parser a -> Parser a -> Parser a
    pf <|> pg = P(\inp -> case parse pf inp of 
                            []        -> parse pg inp
                            [(x,out)] -> [(x,out)])



-- process one character (item) at a time
item :: Parser Char
item = P (\inp -> case inp of
                    []     -> []
                    (x:xs) -> [(x,xs)])
                   
-- conditional item parse
citem :: (Char -> Bool) -> Parser Char
citem p = item >>= \x -> if (p x) then return x else empty -- if fail, return init state.

matchChar :: Char -> Parser Char
matchChar c = citem (==c) 

matchStr :: String -> Parser String
matchStr [] = return []
matchStr (x:xs) = matchChar x >> matchStr xs >> return (x:xs)
                     
matchInt :: Parser Int 
matchInt = (matchChar '-' >> some(citem(isDigit)) >>= \str -> return (read ("-"++str)))
          <|> (some(citem(isDigit)) >>= \str -> return (read str))

space :: Parser ()
space = many (citem isSpace) >> return ()

token :: Parser a -> Parser a
token p = space >> p >>= \val -> space >> return val

symbol :: String -> Parser String
symbol str = token (matchStr str)

int :: Parser Int
int = token (matchInt)

-- left-associative parser grammar
-- expr := (expr| null) +|- term  -> use subexpr to solve infinite loop
-- term := expl *|/ (term| null)
-- expl := factor ^/rt (expl | null)
-- factor := (factor) | int
-- int = ... | -1 | 0 | 1 | ...


subexpr :: Parser (Int->Int)
subexpr  
  = (symbol "+" >> term >>= \x -> return (x+))
  <|> (symbol "-" >> term >>= \x -> return ((-x)+))
  <|> (term >>= \x -> return (x+))


expr :: Parser Int
expr = (many subexpr) >>= \ops -> return (foldl (\acc x -> x acc) 0 ops)
          

term :: Parser Int
term = expl >>= \x ->  (symbol "*" >> term >>= \y -> return (x*y))
                    <|> (symbol "/" >> term >>= \y -> return (x`div`y))
                    <|> (return x)


expl :: Parser Int
expl = factor>>= \x -> (symbol "^" >> expl >>= \y -> return (x^y))
                  <|> (return x)


factor :: Parser Int
factor = (symbol "(" >> expr >>= \x -> symbol ")" >> return x) <|> int

evalP :: String -> Int
evalP str = case parse expr str of
               []       -> 0
               [(n,_)] -> n
