{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RebindableSyntax     #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Language.ArrayForth.HLL.AST where

import           Prelude hiding (Ord (..), Eq (..), not)
import qualified Prelude

import           Control.Monad.Free

import           Data.String

import           Language.ArrayForth.Opcode (F18Word)

data Expr = Num F18Word          
          | ArrayRef String      
          | Array String [F18Word] 
          | Nil                  
          | Op Operator AST AST  
          | UOp UOperator AST    
          | If AST AST AST       
          | For AST AST AST      
          | While AST AST        
          | Map AST AST AST      
          | Fold AST AST AST AST 
          deriving Show

data Forth next = Forth Expr next deriving (Functor, Show)

type AST = Free Forth ()

data Operator = Add | Sub | Mul | Lt | Gt | LtE | GtE | Eq | NEq | Set deriving (Show, Prelude.Eq)

data UOperator = Neg | Not | Get deriving Show

liftExpr :: Expr -> AST
liftExpr expr = liftF $ Forth expr ()

op :: Operator -> AST -> AST -> AST
op opr e₁ e₂ = liftExpr $ Op opr e₁ e₂

instance Num AST where
  fromInteger = liftExpr . Num . fromInteger
  (+) = op Add
  (-) = op Sub
  (*) = op Mul
  negate (Free (Forth (Num n) (Pure ()))) = Free $ Forth (Num $ negate n) (Pure ())
  negate expr = liftExpr $ UOp Neg expr
  abs = undefined
  signum = undefined
  
instance IsString AST where
  fromString = ref

(<), (>), (<=), (≤), (>=), (≥), (==), (/=), (≠), (!), (=:) :: AST -> AST -> AST
(<) = op Lt
(>) = op Gt
(<=) = op LtE
(≤) = (<=)
(>=) = op GtE
(≥) = (>=)
(==) = op Eq
(/=) = op NEq
(≠) = (/=)
(!) = (+)
(=:) = op Set

not, val :: AST -> AST
not = liftExpr . UOp Not
val = liftExpr . UOp Get

ifThenElse :: AST -> AST -> AST -> AST
ifThenElse cond e₁ e₂ = liftExpr $ If cond e₁ e₂

array :: String -> [F18Word] -> AST
array name values = liftExpr $ Array name values

nil :: AST
nil = liftExpr Nil

for :: AST -> AST -> AST -> AST
for var range body = liftExpr $ For var range body

while :: AST -> AST -> AST
while cond body = liftExpr $ While cond body

map :: AST -> AST -> AST -> AST
map var arr body = liftExpr $ Map var arr body

fold :: AST -> AST -> AST -> AST -> AST
fold var₁ var₂ arr body = liftExpr $ Fold var₁ var₂ arr body

ref :: String -> AST
ref name = liftExpr $ ArrayRef name
