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

-- You can look at any AST as a native program using `asNative ast'
-- You can also use `asBits ast'

-- A simple example program. This shows how to use variables,
-- arithmetic and if statements.
test :: AST
test = do "a" =: 10
          "b" =: 2
          "c" =: "a" + "b"
          if "c" == 3 then "d" =: 3 else "d" =: 4

-- We can also have arrays and array indexing with (!). However, no
-- loops quite yet.
test' :: AST
test' = do "arr" `array` [1..5] -- declare an array with value [1,2,3,4,5]
           "arr" ! 3            -- get the fourth element of the array
           "arr" ! 3 =: 10      -- set the fourth element of the array to 10

-- You can add language "constructs" pretty easily.
when :: AST -> AST -> AST
when cond expr = if cond then expr else nil

-- This is how you would use the new "when" construct:
testWhen :: AST
testWhen = "a" =: 10 >> "i" =: 0 >> (when ("a" > 5) $ do "a" =: (- "a")
                                                         "i" =: ("i" + 1))

testFoo :: AST
testFoo = do "input" =: 10
             "x" =: "input" - 2
             "count" =: 0
             when ("x" > 0) $
               do "x" =: "x" - 3
                  "count" =: "count" + 1

asNative :: AST -> NativeProgram
asNative = compiledToNative . compile 

asBits :: AST -> [F18Word]
asBits = fmap toBits . asNative