---Chanan Suksangium CS320
module Tree
 where
import Unify (Subst(S), Substitutable, Unifiable, get, subst, unify, cmb, sub, emp, resolve)
import Equation (Equation (Equals), solveEqn, solveSystem)

data Tree = Leaf | Node Tree Tree | Var String
 deriving (Show, Eq)

instance Substitutable Tree
 where
  subst a Leaf = Leaf
  subst a (Node left right) = Node (subst a left) (subst a right)
  subst a (Var str) = case (get str a) of
   (Just s0) -> s0
   Nothing -> Var str

instance Unifiable Tree
 where
  unify Leaf Leaf = Just emp
  unify (Var x) Leaf = Just (sub x Leaf)
  unify (Var x) (Node y y') = Just (sub x (Node y y'))
  unify (Node y y') (Var x) = Just (sub x (Node y y'))
  unify Leaf (Var y) = Just (sub y Leaf)
  unify (Var x) (Var y) = Just (sub x (Var y))
  unify (Node x x') (Node y y') = cmb (unify x y) (unify x' y')
  unify (Node x x') Leaf = Nothing
  unify Leaf (Node x x') = Nothing

e0 = Node (Node (Node (Var "x") (Var "y")) (Node (Var "y") (Var "x"))) (Var "z")
     `Equals`
     Node (Node (Node Leaf (Var "z")) (Node Leaf (Var "y"))) (Var "x")


e1 = let f b 0 = b
         f b n = Node (f b (n-1)) (f b (n-1))
     in f (Var "x") 10 `Equals` f Leaf 13

e2 = [ (Var "z") `Equals` Leaf
     , Node (Var "y") Leaf `Equals` Node Leaf (Var "x")
     , (Var "x") `Equals` Node (Var "z") (Var "z")]


  
---s0 = Just (S [("z",Var "x"),("x",Leaf),("y",Leaf)])
---s1 = Just (S [("x",Node (Node (Node Leaf Leaf) (Node Leaf Leaf)) (Node (Node Leaf Leaf) (Node Leaf Leaf)))])
---s2 = Nothing
