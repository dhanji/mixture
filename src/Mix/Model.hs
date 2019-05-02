module Mix.Model where

import System.IO
import Data.Array
import Data.Char
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
  -- bits in MIX are not well-defined,
  -- in our case we treat it as Int <10
  -- The first bit is always the sign bit
  bits  :: [Bit]
}

instance Show Cell where
  show cell = [i|#{sign (bits cell !! 0)}#{toBitString "" $ bits cell}|]
    where
      toBitString :: String -> [Bit] -> String
      toBitString string []       = string
      toBitString string (b:bs)   = toBitString (string ++ (show b)) bs
      sign :: Bit -> String
      sign 0  = "+"
      sign _  = "-"


-- A range specified over a Word.
data FieldSpec = FieldSpec Int Int

instance Read FieldSpec where
  readsPrec _ ('(':l:':':h:")") =
    let low = ord l - ord '0'
        hi  = ord h - ord '0' in
      [(FieldSpec low hi, "")]

-- Purely for symmetry with the Read instance
instance Show FieldSpec where
  show (FieldSpec low hi)  = [i|(#{low}:#{hi})|]


data Mix = Mix {
  -- registers
    rA :: Cell
  , rX :: Cell

  , memory  :: Array Int Cell
  , source :: [String]
}

instance Show Mix where
  show mix = [i|
Final MIX Machine state:

register A: #{show $ rA mix}
register X: #{show $ rX mix}

program:
|] ++ (foldl (++) "" [line ++ "\n" | line <- source mix])


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
newCell = Cell { bits = Prelude.replicate 6 0 }


-- Constructs a blank Mix computer.
newMix :: Mix
newMix = Mix {
    rA = newCell
  , rX = newCell
    -- Mix computers have 4000 words of memory.
  , memory = array (1, 4000) []
  , source = []
  }
