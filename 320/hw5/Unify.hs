---Chanan Suksangium CS320
module Unify (Subst(S), Substitutable, Unifiable, emp, sub, get, subst, unify, cmb, resolve, unresolved)
 where

data Subst a = S [(String, a)]
 deriving (Show, Eq)

emp :: Subst a
emp = S []

sub :: String -> a -> Subst a 
sub str a = S [(str, a)]

get :: String -> Subst a -> Maybe a
get str (S[]) = Nothing
get str (S (x:xs))
 |str == fst x = Just (snd x)
 |otherwise = get str (S xs)

unresolved :: [(String,a)] -> Maybe (String, a, a)
unresolved [] = Nothing
unresolved [_] = Nothing
unresolved (x:xs)
 |length(temp' x xs) == 0 = unresolved xs
 |otherwise = Just (head(temp' x xs))
 where
  temp' x xs
   |length(x:xs) == 1 = []
   |fst x == fst (head xs) = [(fst (head xs), snd x, snd (head xs))]
   |otherwise = temp' x (tail xs)

resolve :: Unifiable a => Subst a -> Maybe (Subst a)
resolve (S xs) = case p of
 Nothing -> Just (S xs)
 Just(x', x1, x2) -> let 
  unif = unify x1 x2
  lst = [y' | y' <- xs, y' /= (x', x1)] in
  case unif of
   Nothing -> Nothing
   Just (S(y)) -> resolve (S (lst ++ y))
 where
  p = unresolved (xs)

cmb :: Unifiable a => Maybe (Subst a) -> Maybe (Subst a) -> Maybe (Subst a)
cmb Nothing _ = Nothing
cmb _ Nothing = Nothing
cmb x y
 |x == y = case (x, y) of
  (Just (S(s1)), Just (S(s2))) -> resolve (S(s1))
 |otherwise = case (x, y) of
  (Just (S(s1)), Just (S(s2))) -> resolve (S(s1 ++ s2))

class Substitutable a where
 subst :: Subst a -> a -> a

class (Eq a, Substitutable a) => Unifiable a where
 unify :: a -> a -> Maybe (Subst a)


