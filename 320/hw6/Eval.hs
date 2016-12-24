----------------------------------------------------------------
-- Computer Science 320 (Fall, 2016)
-- Concepts of Programming Languages
-- Chanan Suksangium
-- Assignment 6
--   Eval.hs

----------------------------------------------------------------
-- Evaluation Functions for the mini-Haskell Interpreter

module Eval (evalExp) where

import Env
import Err (Error(..))
import Exp (Oper(..), Exp(..))
import Val (Val(..))

----------------------------------------------------------------
-- This function is exported to the Main module.

evalExp :: Exp -> Error Val
evalExp e = ev e emptyEnv

----------------------------------------------------------------
-- Functions for evaluating operations applied to values.

appOp :: Oper -> Val -> Error Val
appOp o (VN v)
 |o `elem` [Plus, Times, Equal, Cons] = S(Partial o (VN v))
appOp And (VB v) = S(Partial And (VB(v)))
appOp Or (VB v) = S(Partial Or (VB(v)))
appOp Equal v = S(Partial Equal v)
appOp Cons (VB v) = S(Partial Cons (VB(v)))
appOp Cons (VN v) = S(Partial Cons (VN(v)))
appOp Not (VB v) = S(VB(not v))
appOp Not _ = Error "Function not defined on variable"
appOp Tail (VListBool []) = Error "List is empty"
appOp Tail (VListBool v) = S(VListBool(tail v))
appOp Tail (VListInt []) = Error "List is empty"
appOp Tail (VListInt v) = S(VListInt(tail v))
appOp Head (VListBool []) = Error "List is empty"
appOp Head (VListBool v) = S(VB(head (v)))
appOp Head (VListInt []) = Error "List is empty"
appOp Head (VListInt v) = S(VN(head (v)))
appOp _ _ = Error "appOp Error"

appBinOp :: Oper -> Val -> Val -> Error Val
appBinOp Plus (VN v) (VN y) = S(VN(v + y))
appBinOp Times (VN v) (VN y) = S(VN(v * y))
appBinOp Equal (VN v) (VN y) = S(VB(v == y))
appBinOp And (VB v) (VB y) = S(VB(v && y))
appBinOp Or (VB v) (VB y) = S(VB(v || y))
appBinOp Equal (VB v) (VB y) = S(VB(v == y))
appBinOp Cons (VN v) (VListInt y) = S(VListInt(v : y))
appBinOp Cons (VB v) (VListBool y) = S(VListBool(v : y))
appBinOp Cons  (VN v) VNil    = S (VListInt (v:[]))
appBinOp Cons  (VB v) VNil    = S (VListBool (v:[]))
appBinOp _ _ _ = Error "Binary Operator Error"
--Equal for list

----------------------------------------------------------------
-- Function for applying one value to another.

appVals :: Val -> Val -> Error Val
appVals (Partial o v) y = appBinOp o v y
appVals (VOp o) y = appOp o y
appVals (VLamUnit e env) v = ev e env
appVals (VLam n e env) v = ev e (updEnv n v env)
appVals _ _ = Error "appVals Error"

----------------------------------------------------------------
-- Function for evaluating an expression with no bindings or
-- variables to a value.

ev0 :: Exp -> Error Val
ev0 Unit = S(VUnit)
ev0 Nil = S(VNil)
ev0 (N e) = S(VN e)
ev0 (B e) = S(VB e)
ev0 (Op e) = S(VOp e)
ev0 (App e1 e2) = case (ev0 e1) of 
 Error err -> Error err
 S v1 -> case (ev0 e2) of
  Error err -> Error err
  S v2 -> appVals v1 v2
ev0 (If e1 e2 e3) = case (ev0 e1) of
 S (VB c)  -> if c then ev0 e2 else ev0 e3
 S _       -> Error "e1 not a boolean"
 Error err -> Error err
ev0 _ = Error "Unable to be Evaluated"

----------------------------------------------------------------
-- Function for evaluating an expression to a value. Note the
-- need for an environment to keep track of variables.

ev :: Exp -> Env Val -> Error Val
ev Unit env = S(VUnit)
ev Nil env = S(VNil)
ev (N e) env = S(VN e)
ev (B e) env = S(VB e)
ev (Op e) env = S(VOp e)
ev (Var e) env = case (findEnv e env) of
 Just(v) -> S(v)
 Nothing -> Error "Variable not bounded in environment"
ev (App e1 e2) env = case (ev e1 env) of 
 Error err -> Error err
 S v1 -> case (ev e2 env) of
  Error err -> Error err
  S v2 -> appVals v1 v2
ev (If e1 e2 e3) env = case (ev e1 env) of
 S (VB c)  -> if c then ev e2 env else ev e3 env
 S _       -> Error "e1 not a boolean"
 Error err -> Error err
ev (Let [] e) env = ev e env
ev (Let ((x,ee):xs) e) env = case ev ee env of
 S(ve) -> ev (Let xs e) (updEnv x ve env) 
 _ -> Error " "
ev (Lam x e) env = S (VLam x e env)
ev (LamUnit e) env = S (VLamUnit e env)
ev _ _ = Error "Evaluation error"
