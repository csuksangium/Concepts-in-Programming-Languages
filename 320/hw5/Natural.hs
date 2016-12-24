---Chanan Suksangium CS320
module Natural
 where
import Unify (Subst(S), Substitutable, Unifiable, get, subst, unify, cmb, sub, emp, resolve, unresolved)

data Natural = Zero | Succ Natural | Var String
 deriving (Show, Eq)

instance Substitutable Natural
 where
  subst a Zero = Zero
  subst a (Succ b) = Succ(subst a b)
  subst a (Var str) = case (get str a) of
   (Just s0) -> s0
   Nothing -> Var str

instance Unifiable Natural
 where
  unify Zero Zero = Just emp
  unify Zero (Var y) = Just (sub y Zero)
  unify (Var x) Zero = Just (sub x Zero)
  unify (Var x) (Succ y) = Just (sub x (Succ y))
  unify (Succ x) (Var y) = Just (sub y (Succ x))
  unify (Succ x) (Succ y) = unify x y
  unify (Var x) (Var y) = Just (sub x (Var y))
  unify Zero (Succ x) = Nothing
  unify (Succ x) Zero = Nothing

---main = do
 ---print(subst (S [("x", Zero)]) (Var "x"))

