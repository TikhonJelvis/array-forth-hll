{-# LANGUAGE NamedFieldPuns #-}
module Language.ArrayForth.Stack where

data Stack a = Stack { dataS, retS :: [a]
                     , a :: a }

data Operation = Dup | Over | Drop | Pop | Push | ToA | FromA deriving (Show, Eq, Bounded, Enum)

operate :: Stack a -> Operation -> Stack a
operate stack@Stack {dataS = d:ds, retS = r:rs, a} opr = case opr of
  Dup   -> stack { dataS = d:d:init ds }
  Drop  -> stack { dataS = ds }
  Push  -> stack { dataS = ds, retS = d:r:rs }
  Pop   -> stack { dataS = r:d:ds, retS = rs }
  ToA   -> stack { dataS = ds, a = d }
  FromA -> stack { dataS = a:d:ds }
  