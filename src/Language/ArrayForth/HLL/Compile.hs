{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.ArrayForth.HLL.Compile where

import           Control.Applicative         ((<$), (<$>), (<*), (<*>))
import           Control.Arrow               (second)
import           Control.Monad.Free          (Free (..))
import           Control.Monad.State         (State, get, gets, modify, put, runState)

import           Data.List                   (genericLength)
import           Data.Maybe                  (isNothing)

import qualified Language.ArrayForth.Opcode  as OP
import qualified Language.ArrayForth.Program as AF

import           Language.ArrayForth.HLL.AST

-- | Wraps a number into a constant instruction.
num :: OP.F18Word -> AF.Instruction
num = AF.Number

-- | A chunk of memory used to store a variable or array.
data Mem = Mem { location, size :: OP.F18Word } deriving (Show, Eq)

data St = St { counter   :: Int
             , vars      :: [(String, Mem)]
             , startData :: [OP.F18Word]
             } deriving Show

nextName :: St -> St
nextName s@St { counter } = s {counter = succ counter}

addArray :: String -> [OP.F18Word] -> State St ()
addArray name values = do state@St { vars, startData } <- get
                          let mem = Mem (genericLength startData) (genericLength values)
                          case lookup name vars of
                            Nothing -> put state { vars = (name, mem) : vars
                                                , startData = startData ++ values }
                            Just{}  -> return ()
                            
compile :: AST -> (AF.Program, [OP.F18Word])
compile ast = second startData . runState (compileAST ast) $ St 0 [] []
  where compileAST (Pure _)                 = return []
        compileAST (Free (Forth expr next)) = (++) <$> go expr <*> compileAST next
          where jump opcode label = AF.Jump opcode $ AF.Abstract label
                labelName = (show <$> gets counter) <* modify nextName

                go (Num n) = return [num n]
                go Nil     = return []
                go (Op Set name value) =
                  do St { vars } <- get
                     case name of
                       Free (Forth (ArrayRef n) (Pure ()))
                         | isNothing $ lookup n vars -> addArray n [0] >> go expr
                       _ -> set <$> compileName name <*> compileAST value
                         where set addr val = addr ++ "b!" ++ val ++ "!b"
                go (Op Index name offset) = do loc <- compileName name
                                               idx <- compileAST offset
                                               return $ loc ++ idx ++ "+"
                go (Op opr e1 e2) = do e1' <- compileAST e1
                                       e2' <- compileAST e2
                                       return $ e1' ++ e2' ++ operator opr
                go (UOp Neg e) = (++ "-") <$> compileAST e
                go (If cond e1 e2) =
                  do cond' <- compileAST cond
                     e1' <- compileAST e1
                     e2' <- compileAST e2
                     end <- labelName
                     yes <- labelName
                     let ifInstr = [if minusIf cond then jump OP.MinusIf yes else jump OP.If yes]
                     return $ cond' ++ ifInstr ++ e2' ++ [jump OP.Jmp end, AF.Label yes] ++
                       e1' ++ [AF.Label end]
                go (Array name values) = [] <$ addArray name values
                go (ArrayRef name) = (++ "b! @b") <$> getName name
                go _ = error "Not implemented yet. Sorry!"

                compileName (Free (Forth (ArrayRef name) (Pure ()))) = getName name
                compileName _ = error "Need a variable name here."

                getName name =
                  do St { vars } <- get
                     case lookup name vars of
                       Just Mem { location } -> return $ [AF.Number location]
                       Nothing               -> error $ "Unknown variable " ++ name                                  
                              
                operator Add = "+"
                operator Sub = "- 1 + +"
                operator Mul = concat $ replicate 18 "+*" -- TODO: implement less stupidly
                operator Lt  = "- 1 + +"
                operator Gt  = "over - 1 + +"
                operator _   = error "This operator not implemented yet."

                minusIf (Free (Forth (Op opr _ _) _)) = opr `elem` [Lt, Gt, LtE, GtE]
                minusIf _                             = False
