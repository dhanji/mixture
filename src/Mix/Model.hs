module Mix.Model where

import System.IO
import Data.Array
import Data.String.Interpolate
import qualified Data.Text as Text
import qualified Data.List.Split as Split


type Byte = Int


data Sign = Plus | Minus

instance Show Sign where
  show Plus   = "+"
  show Minus  = "-"


-- In this case a MIX word is basically the structure of a register.
data Cell = Cell {
    sign  :: Sign
  , bits  :: [Byte]
}

instance Show Cell where
  show word = [i|#{sign word}#{toBitString "" $ bits word}|]
    where
      toBitString :: String -> [Byte] -> String
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

data Op = Load | Store deriving (Eq)


data Instruction = Comment | Blank | Instruction {
    op      :: Op
  -- the target register, i.e. A1-5 or X1-5
  , target  :: String

  , address :: Int

  -- The index and field specifications I and F as per MIX code.
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
    memory = newMemory
  }
  where
    -- Mix computers have 4000 words of memory.
    newMemory   = array (1, 4000) []
