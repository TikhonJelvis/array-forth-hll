{-# LANGUAGE OverloadedStrings #-}
module Language.ArrayForth.HLL.Compile where

import           Control.Monad.Free

import qualified Language.ArrayForth.Opcode  as OP
import qualified Language.ArrayForth.Program as AF

import           Language.ArrayForth.HLL.AST

-- | Wraps a number into a constant instruction.
num :: OP.F18Word -> AF.Instruction
num = AF.Number

compile :: AST -> AF.Program
compile (Pure _)                 = []
compile (Free (Forth expr next)) = go expr ++ compile next
  where go (Num n) = [num n]
        go Nil = []
        go (Op opr e1 e2) = compile e1 ++ compile e2 ++ operator opr
        go (UOp Neg e) = compile e ++ "-"
        go (If cond e1 e2) = compile cond ++ ifInstr ++ " :yes" ++ compile e2 ++
                             "jump :end :yes" ++ compile e1 ++ ":end"
          where ifInstr = if minusIf cond then "-if" else "if"

        operator Add = "+"
        operator Sub = "- 1 + +"
        operator Mul = "18 push +* unext"
        operator _ = error "Only arithmetic operators allowed here."

        minusIf (Free (Forth (Op opr _ _) _)) = opr `elem` [Lt, Gt, LtE, GtE]
        minusIf _                             = False