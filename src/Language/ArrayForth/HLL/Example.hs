{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}
module Language.ArrayForth.HLL.Example where

import           Prelude                         hiding (Eq (..), Ord (..))

import           Data.String                     (IsString (..))

import           Language.ArrayForth.Program     (toNative)

import           Language.ArrayForth.HLL.AST
import           Language.ArrayForth.HLL.Compile

test = do "a" =: 10
          if "a" > 2 then 1 + 2 else 2 + 3
