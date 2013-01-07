{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}
module Language.ArrayForth.HLL.Example where

import           Prelude                           hiding (Eq (..), Ord (..), map)

import           Data.String                       (IsString (..))

import           Language.ArrayForth.NativeProgram (Instrs (Constant),
                                                    NativeProgram, toBits)
import           Language.ArrayForth.Opcode        (F18Word)
import           Language.ArrayForth.Program       (Program, toNative)

import           Language.ArrayForth.HLL.AST
import           Language.ArrayForth.HLL.Compile

compiledToNative :: (Program, [F18Word]) -> NativeProgram
compiledToNative (program, memory) = fmap Constant memory ++ toNative program

test :: AST
test = do "a" =: 1
          "b" =: 2
          if "a" > "b" then "c" =: 3 else "c" =: 4

-- You can add language "constructs" pretty easily.
when :: AST -> AST -> AST
when cond expr = if cond then expr else nil

asNative :: NativeProgram
asNative = compiledToNative $ compile test

asBits :: [F18Word]
asBits = fmap toBits asNative