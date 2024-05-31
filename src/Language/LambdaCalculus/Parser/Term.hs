{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Language.LambdaCalculus.Parser.Term
  ( parseLC
  ) where

-- | See http://mattwetmore.me/posts/parsing-combinators-with-parser-combinators.html
import Data.List (elemIndex, isPrefixOf)

import Language.LambdaCalculus.AST
import Language.LambdaCalculus.Parser.Common

import Text.Parsec

parseAbs :: LCParser Term
parseAbs = do
  pos <- getPosition
  backslash
  v <- identifier
  modifyState (v :)
  dot
  term <- parseTerm
  modifyState tail
  return $ TmAbs (infoFrom pos) v term

parseVar :: LCParser Term
parseVar = do
  v <- identifier
  list <- getState
  findVar v list

findVar :: String -> BoundContext -> LCParser Term
findVar v list = case elemIndex v list of
  Nothing -> fail $ "The variable " ++ v ++ " has not been bound"
  Just n  -> do
    pos <- getPosition
    return $ TmVar (infoFrom pos) n (length list)

parseNonApp :: LCParser Term
parseNonApp =  parens parseTerm   -- (M)
           <|> parseAbs           -- $\lambda$x.M
           <|> parseVar           -- x

parseTerm :: LCParser Term
parseTerm = chainl1 parseNonApp $ do
  whiteSpace
  pos <- getPosition
  return $ TmApp (infoFrom pos)

parseWith :: Parsec String [u] a -> String -> Either ParseError a
parseWith p = runParser p [] "untyped lambda-calculus"

replaceLambda :: String -> String
replaceLambda = go
  where
    go [] = []
    go xs
      | "id" `isPrefixOf` xs = "\\x.x" ++ go (drop 2 xs)
      | "tru" `isPrefixOf` xs = "\\t.\\f. t" ++ go (drop 3 xs)
      | "fls" `isPrefixOf` xs = "\\t.\\f. f" ++ go (drop 3 xs)
      | "scc" `isPrefixOf` xs = "\\n.\\s.\\z. s (n s z)" ++ go (drop 3 xs)
      | "plus" `isPrefixOf` xs = "\\m.\\n.\\s.\\z. m s (n s z)" ++ go (drop 4 xs)
      | "zero" `isPrefixOf` xs = "\\s.\\z. z" ++ go (drop 4 xs)
      | "one" `isPrefixOf` xs = "\\s.\\z. s z" ++ go (drop 3 xs)
      | "two" `isPrefixOf` xs = "\\s.\\z. s (s z)" ++ go (drop 3 xs)
      | "three" `isPrefixOf` xs = "\\s.\\z. s (s (s z))" ++ go (drop 5 xs)
      | "test" `isPrefixOf` xs = "\\l.\\m.\\.n (l m n)" ++ go (drop 4 xs)
      | otherwise = head xs : go (tail xs)

parseLC :: String -> Either ParseError Term
parseLC s = do
  let s1 = replaceLambda s
  parseWith (whiteSpace >> parseTerm) s1
