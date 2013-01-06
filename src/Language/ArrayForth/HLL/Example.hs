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

test = do "a" =: 10
          if "a" > 2 then 1 + 2 else 2 + 3

asNative = compiledToNative $ compile test

asBits = fmap toBits $ asNative