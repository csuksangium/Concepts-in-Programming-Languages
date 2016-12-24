----------------------------------------------------------------
-- Computer Science 320 (Fall, 2016)
-- Concepts of Programming Languages
--
-- Assignment 7
--   Exp.hs

----------------------------------------------------------------
-- Abstract Syntax for the mini-Haskell Interpreter

module Exp (Oper(..), Exp(..), opsWithStrings, opsWithStrings2, subst) 
  where

data Oper = Plus | Times 
          | Equal 
          | And | Or | Not 
          | Head | Tail | Cons
  deriving Eq

data Exp = Unit | Nil
         | N Int | B Bool | Var String | Op Oper
         | App Exp Exp
         | LamUnit Exp
         | Lam String Exp
         | If Exp Exp Exp
         | Let [(String, Exp)] Exp

----------------------------------------------------------------
-- Printing functions for Abstract Syntax

ops       = [Not, Head, Tail,
             Plus, Times, Equal, And, Or, Cons]
opStrings = ["not", "head", "tail",
             "(+)", "(*)", "(==)", "(&&)", "(||)", "(:)"]
opsWithStrings = zip ops opStrings

showOper op = fOp op opsWithStrings
  where
    fOp op ((o,n):ons) = if (o == op) then n else (fOp op ons)
    fOp op []           = "impossible"

-- Convenient list for the parser.
ops2       = [Plus, Times, Equal, And, Or, Cons]
opStrings2 = ["+)", "*)", "==)", "&&)", "||)", ":)"]
opsWithStrings2 = zip ops2 opStrings2

showExp wh Unit  = wh ++ "()"
showExp wh Nil   = wh ++ "[]"
showExp wh (N n) = wh ++ (show n)
showExp wh (B b) = wh ++ (show b)
showExp wh (Var x) = wh ++ x
showExp wh (Op o) = wh ++ (show o)
showExp wh (App e1 e2) = "(" ++ (showExp wh e1) ++ 
                         " " ++ (showExp wh e2) ++ ")"
showExp wh (LamUnit e) = "\\() -> " ++ (showExp wh e)
showExp wh (Lam x e) = "\\" ++ x ++ " -> " ++ (showExp wh e)
showExp wh (If e1 e2 e3) = "if " ++ (showExp wh e1) ++ 
                           " then " ++ (showExp wh e2) ++ 
                           " else " ++ (showExp wh e3)
showExp wh (Let xses e) = "let\n" ++ (showBinds wh xses) ++ 
                          "in \n  " ++ (showExp wh e)

showBinds wh [] = "impossible"
showBinds wh [(n, e)] = "  " ++ n ++ " = " ++ (showExp wh e) ++ "\n"
showBinds wh ((n, e):xns) = "  " ++ n ++ " = " ++ (showExp wh e) ++ 
                                 ";\n" ++ (showBinds wh xns)

instance Show Oper where
  show = showOper

instance Show Exp where
  show = showExp []


noLets :: Exp -> Exp
noLets (Let [] e') = noLets e'
noLets (Let ((x, e):xs) e') = App (Lam x (noLets (Let xs e'))) (e)
noLets (App e1 e2) = App (noLets e1) (noLets e2)
noLets (LamUnit e1) = LamUnit (noLets e1)
noLets (Lam str e1) = Lam str (noLets e1)
noLets (If e1 e2 e3) = If (noLets e1) (noLets e2) (noLets e3)
noLets exp = exp

subst :: String -> Exp -> Exp -> Exp
subst s e1 (Var s1)     = if (s == s1) then e1 else Var s1
subst s e1 (If e2 e3 e4) = If (subst s e1 e2) (subst s e1 e3) (subst s e1 e4)
subst s e1 (App e2 e3)   = App (subst s e1 e2) (subst s e1 e3)
subst s e1 (LamUnit e2) = LamUnit (subst s e1 e2)
subst s e1 (Lam s' e2)
 |s == s' = (Lam s' e2)
 |otherwise = (Lam s' (subst s e1 e2))
subst s e1 (Let xs e2) = subst s e1 (noLets (Let xs e2))
subst s e1 e = e