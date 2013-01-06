{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.ArrayForth.HLL.Compile where

import           Control.Applicative         ((<$), (<$>), (<*), (<*>))
import           Control.Monad.Free          (Free (..))
import           Control.Monad.State

import           Data.List                   (genericLength)

import Debug.Trace (trace)

import qualified Language.ArrayForth.Opcode  as OP
import qualified Language.ArrayForth.Program as AF

import           Language.ArrayForth.HLL.AST

-- | Wraps a number into a constant instruction.
num :: OP.F18Word -> AF.Instruction
num = AF.Number

data St = St { counter   :: Int
             , vars      :: [(String, OP.F18Word)]
             , startData :: [OP.F18Word]
             } deriving Show

nextName :: St -> St
nextName s@St { counter } = s {counter = succ counter}

addArray :: String -> [OP.F18Word] -> State St ()
addArray name values = do state@St { vars, startData } <- get
                          case lookup name vars of
                            Nothing -> put state { vars = (name, genericLength startData) : vars
                                                , startData = startData ++ values }
                            Just{}  -> return ()

compile :: AST -> AF.Program
compile ast = evalState (compileAST ast) $ St 0 [] []
  where compileAST (Pure _)                 = return []
        compileAST (Free (Forth expr next)) = (++) <$> go expr <*> compileAST next
          where jump opcode label = AF.Jump opcode $ AF.Abstract label
                name = (show <$> gets counter) <* modify nextName

                go (Num n) = return [num n]
                go Nil = return []
                go expr@(Op Set aRef value) =
                  do let Free (Forth (ArrayRef name) (Pure ())) = aRef
                     st@St { vars } <- get
                     case lookup name vars of
                       Just addr -> do value' <- compileAST value
                                       return $ AF.Number addr : "b!" ++ value' ++ "!b"
                       Nothing -> addArray name [0] >> go expr
                go (Op opr e1 e2) = do e1' <- compileAST e1
                                       e2' <- compileAST e2
                                       return $ e1' ++ e2' ++ operator opr
                go (UOp Neg e) = (++ "-") <$> compileAST e
                go (UOp Get e) = (++ "b! @b") <$> compileAST e
                go (If cond e1 e2) =
                  do cond' <- compileAST cond
                     e1' <- compileAST e1
                     e2' <- compileAST e2
                     end <- name
                     yes <- name
                     let ifInstr = [if minusIf cond then jump OP.MinusIf yes else jump OP.If yes]
                     return $ cond' ++ ifInstr ++ e2' ++ [jump OP.Jmp end, AF.Label yes] ++
                       e1' ++ [AF.Label end]
                go (Array name values) = [] <$ addArray name values
                go (ArrayRef name) = do addr <- lookup name <$> gets vars
                                        case addr of
                                          Just a -> return $ [AF.Number a]
                                          Nothing -> error $ "Unknown variable " ++ name

                operator Add = "+"
                operator Sub = "- 1 + +"
                operator Mul = concat $ replicate 18 "+*" -- TODO: implement less stupidly
                operator Lt  = "- 1 + +"
                operator Gt  = "over - 1 + +"
                operator _   = error "This operator not implemented yet."

                minusIf (Free (Forth (Op opr _ _) _)) = opr `elem` [Lt, Gt, LtE, GtE]
                minusIf _                             = False
