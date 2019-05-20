module Mix.Data where

import System.IO
import Data.Array


type Byte = Int


-- constants.
cellWidth :: Int
cellWidth = 5


data Sign = Plus | Minus


-- a MIX cell is basically a memory location (including a register)
data Cell = Cell {
    sign   :: Sign
  -- bits in MIX are not well-defined,
  -- in our case we treat it as Int <10
  -- The first bit is always the sign bit
  , bytes  :: [Byte]
}


-- A range specified over a Word.
data FieldSpec = FieldSpec Int Int


data Mix = Mix {
  -- registers
    rA :: Cell
  , rX :: Cell
  , rJ :: Cell
  , rI :: [Cell]

  , memory      :: Array Int Cell
  , source      :: [String]
}


data Op = Load | Store | Zero | Increment | Set | Add | Sub | Mul | Div deriving (Eq)


data Instruction = Comment | Blank | Instruction {
    op      :: Op
  -- the target register (if any), i.e. A1-5 or X1-5
  , target  :: Maybe String
  , sign    :: Sign
  , address :: Int

  -- The index and field specifications I and F as per MIX assembly.
  , iSpec   :: Int
  , fSpec   :: FieldSpec
}
