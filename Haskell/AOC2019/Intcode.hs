module Intcode where

import qualified Data.Map.Strict as M

import Data.List.Split (splitOn)

data State = Ready
           | Running
           | Idle
           | Crash
           | Halt
  deriving (Show)

data Opcode = Oadd
            | Omul
            | Ohlt

data Instruction = Instruction Opcode Int

type Memory = M.Map Int Int

parseCode :: String -> Memory
parseCode = M.fromList . zip [0 .. ] . map read . splitOn ","

data Machine = Machine { memory   :: Memory
                       , state    :: State
                       , inQueue  :: [Int]
                       , outQueue :: [Int]
                       , relBase  :: Int
                       , memPtr   :: Int
                       }

newMachine :: String -> Machine
newMachine s = Machine { memory = parseCode s
                       , state = Ready
                       , inQueue = []
                       , outQueue = []
                       , relBase = 0
                       , memPtr = 0
                       }

sendInput :: Int -> Machine -> Machine
sendInput x m = m {inQueue = q}
  where
    q = inQueue m ++ [x]

sendInputs :: [Int] -> Machine -> Machine
sendInputs xs m = m {inQueue = q}
  where
    q = inQueue m ++ xs

runMachine :: Machine -> Machine
runMachine m = undefined

stepMachine :: Machine -> Machine
stepMachine m = case op of
  Oadd -> undefined
  Omul -> undefined
  Ohlt -> undefined
  where
    op = decode $ viewMemCell ptr m
    ptr = memPtr m

decode :: Int -> Opcode
decode i = case mod i 100 of 
  1  -> Oadd
  2  -> Omul
  99 -> Ohlt
  _  -> error "Invalid opcode"

viewMemCell :: Int -> Machine -> Int
viewMemCell i = maybe (error "Invalid cell index") id . M.lookup i . memory

