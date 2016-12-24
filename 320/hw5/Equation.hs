---Chanan Suksangium CS320
module Equation (Equation (Equals), solveEqn, solveSystem)
 where
import Unify (Subst(S), Substitutable, Unifiable, get, subst, unify, cmb, emp, sub, resolve, unresolved)

data Equation a = a `Equals` a

solveEqn :: (Unifiable a) => Equation a -> Maybe (Subst a)
solveEqn (a `Equals` b) = unify a b

solveSystem :: (Unifiable a) => [Equation a] -> Maybe (Subst a)
solveSystem xs
 |length(lst') < length(xs) = Nothing
 |otherwise = resolve (S(lst'))
 where
  lst = map solveEqn xs
  lst' = solveSystemHelper lst

solveSystemHelper :: (Unifiable a) => [Maybe (Subst a)] -> [(String, a)]
solveSystemHelper [] = []
solveSystemHelper (x:xs) = case x of
 Just(S(s)) -> s ++ solveSystemHelper xs
 Nothing -> []

