module Mix.Model where

import System.IO
import Data.Array
import Data.String.Interpolate
import qualified Data.Text as Text
import qualified Data.List.Split as Split


type Bit = Int


data Sign = Plus | Minus

instance Show Sign where
  show Plus   = "+"
  show Minus  = "-"


-- a MIX cell is basically a memory location (including a register)
data Cell = Cell {
    sign  :: Sign

    -- bits in MIX are not well-defined,
    -- in our case we treat it as Int <10
  , bits  :: [Bit]
}

instance Show Cell where
  show cell = [i|#{sign cell}#{toBitString "" $ bits cell}|]
    where
      toBitString :: String -> [Bit] -> String
      toBitString string []       = string
      toBitString string (b:bs)   = toBitString (string ++ (show b)) bs


-- A range specified over a Word.
data FieldSpec = FieldSpec Int Int


data Mix = Mix {
  -- registers
    rA :: Cell
  , rX :: Cell

  , memory :: Array Int Cell
}

instance Show Mix where
  show mix = [i|
Final MIX Machine state:

register A: #{show $ rA mix}
register X: #{show $ rX mix}
|]

data Op = Load | Store | Zero | Add deriving (Eq)


data Instruction = Comment | Blank | Instruction {
    op      :: Op
  -- the target register (if any), i.e. A1-5 or X1-5
  , target  :: Maybe String
  , address :: Int

  -- The index and field specifications I and F as per MIX assembly.
  , iSpec   :: Int
  , fSpec   :: FieldSpec
}


-- Constructs a blank cell suitable for use in registers or main memory.
newCell :: Cell
newCell = Cell { sign = Plus, bits = Prelude.replicate 5 0 }


-- Constructs a blank Mix computer.
newMix :: Mix
newMix = Mix {
    rA = newCell,
    rX = newCell,
    -- Mix computers have 4000 words of memory.
    memory = array (1, 4000) []
  }
