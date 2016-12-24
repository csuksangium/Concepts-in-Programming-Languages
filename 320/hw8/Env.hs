----------------------------------------------------------------
-- Computer Science 320 (Fall, 2016)
-- Concepts of Programming Languages
-- Chanan Suksangium
-- Assignment 6
--   Env.hs

----------------------------------------------------------------
-- Environments

module Env (Env, emptyEnv, updEnv, findEnv)

 where

type Env a = [(String, a)] --Problem #2 (a) Not Yet Implemented

emptyEnv :: Env a
emptyEnv = []

updEnv :: String -> a -> Env a -> Env a
updEnv str b xs = [(str,b)] ++ xs


findEnv :: String -> Env a -> Maybe a
findEnv str [] = Nothing
findEnv str (x:xs)
 |str == (fst x) = Just (snd x)
 |otherwise = findEnv str (xs)