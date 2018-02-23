{-# LANGUAGE InstanceSigs #-}
module GraphQL where

newtype Parser a = Parser
  { parse :: String -> [(a, String)]
  }

item :: Parser Char
item =
  Parser
    (\cs ->
       case cs of
         ""     -> []
         (x:xs) -> [(x, xs)])

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  f `fmap` fa = Parser (\cs -> [(f a, cs') | (a, cs') <- fa `parse` cs])

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser (\cs -> [(x, cs)])

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> pa = Parser (\cs -> [(f a, cs'') | (f, cs') <- pf `parse` cs, (a, cs'') <- pa `parse` cs'])

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  pa >>= f = Parser (\cs -> [(b, cs'') | (a, cs') <- pa `parse` cs, (b, cs'') <- (f a) `parse` cs'])
