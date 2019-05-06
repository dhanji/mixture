module Mix.Model where

import System.IO
import Data.Array
import Data.Char
import Data.String.Interpolate
import qualified Data.Text as Text
import qualified Data.List.Split as Split


type Byte = Int


-- constants.
cellWidth :: Int
cellWidth = 5


data Sign = Plus | Minus

instance Show Sign where
  show Plus   = "+"
  show Minus  = "-"

instance Read Sign where
  readsPrec _ "-" = [(Minus, "")]
  readsPrec _ _   = [(Plus, "")]


-- a MIX cell is basically a memory location (including a register)
data Cell = Cell {
    sign   :: Sign
  -- bits in MIX are not well-defined,
  -- in our case we treat it as Int <10
  -- The first bit is always the sign bit
  , bytes  :: [Byte]
}

instance Show Cell where
  show cell = [i|#{sign (cell :: Cell)}#{toBitString $ bytes cell}|]
    where
      toBitString = foldl (++) "" . map show


-- A range specified over a Word.
data FieldSpec = FieldSpec Int Int

instance Read FieldSpec where
  readsPrec _ ('(':l:':':h:")") =
    let low = ord l - ord '0'
        hi  = ord h - ord '0' in
      [(FieldSpec low hi, "")]
  readsPrec _ _                 = [(FieldSpec 0 5, "")]

-- Purely for symmetry with the Read instance
instance Show FieldSpec where
  show (FieldSpec low hi)  = [i|(#{low}:#{hi})|]


data Mix = Mix {
  -- registers
    rA :: Cell
  , rX :: Cell
  , rJ :: Cell
  , rI :: [Cell]

  , memory      :: Array Int Cell
  , source      :: [String]
}

instance Show Mix where
  show mix = [i|

      register A              register X
      #{show $ rA mix}                 #{show $ rX mix}

  I registers:
  #{map show (rI mix)}

  register J:
  #{show $ rJ mix}


assembly:
|] ++ (foldl (++) "" [line ++ "\n" | line <- source mix])


data Op = Load | Store | Zero | Increment | Set deriving (Eq)


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


-- Constructs a blank cell suitable for use in registers or main memory.
newCell :: Cell
newCell = Cell {
    sign  = Plus
  , bytes = replicate cellWidth 0
}


-- Constructs a blank Mix computer.
newMix :: Mix
newMix = Mix {
    rA = newCell
  , rX = newCell
  , rJ = newCell
  , rI = replicate 6 newCell
    -- Mix computers have 4000 words of memory.
  , memory      = array (1, 4000) [(i, newCell)| i <- [1..4000]]
  , source      = []
}


-- Returns a slice of the given bitstring according to the FieldSpec
-- Contents of source are copied to a new cell such that
-- any bits of target that are not covered by the F-spec
-- are "preserved" intact in the result and shifted to the right.
copyCellRight :: FieldSpec -> Cell -> Cell -> Cell
copyCellRight (FieldSpec low to) Cell{sign=ssign,bytes=sbytes} Cell{sign=tsign,bytes=tbytes} = Cell {
    sign  = if | low == 0   -> ssign
               | otherwise  -> tsign
  , bytes = pad tbytes ++ slice sbytes
}
  where
    from              = low - 1
    pad               = take (cellWidth - length (slice sbytes))
    slice | low == 0  = take (to - from - 1)
          | otherwise = take (to - from) . drop from


-- Returns a slice of the given bitstring according to the FieldSpec
-- Contents of source are copied to a new cell such that
-- any bits of target that are not covered by the F-spec
-- are "preserved" intact in the result and shifted to the left.
copyCellLeft :: FieldSpec -> Cell -> Cell -> Cell
copyCellLeft (FieldSpec from to) source target = Cell {
    sign = signbit
  , bytes = pad ++ slice
}
  where
    pad             = take (cellWidth - length slice) (bytes target)
    signbit
        | from == 0 = sign (source :: Cell)
        | otherwise = sign (target :: Cell)
    slice
        | to == 0   = [] -- sign bit only.
        | from == 0 = take to $ bytes source
        | otherwise = take (to - from + 1) $ drop (from - 1) (bytes source)


-- Converts an Int to a cellWidth-word Byte string suitable for use in a cell.
toBytes :: Int -> [Byte]
toBytes i | i == 0    = replicate cellWidth 0
          | i > 0     = bytize i
          | otherwise = 1:(bytize $ abs i)
  where
    bytize i = [(i `mod` 10^n) `div` 10^(n - 1) | n <- [cellWidth,cellWidth - 1..1]]


-- Destructively sets the value of a register to the given cell.
set :: Mix -> Maybe String -> Cell -> Mix
set mix Nothing _       = mix
set mix (Just reg) cell = case reg of
    "A"      -> mix { rA = cell }
    "X"      -> mix { rX = cell }
    "J"      -> mix { rJ = cell }
    ('I':i)  -> let ix = read i in mix { rI = replace ix }
  where
    replace ix = [if j == ix then cell else v | j <- [1..cellWidth], v <- rI mix]


-- Returns the value of the register identified by the given String
get :: Mix -> Maybe String -> Cell
get mix Nothing    = newCell
get mix (Just reg) = case reg of
    "A"      -> rA mix
    "X"      -> rX mix
    "J"      -> rJ mix
    ('I':i)  -> rI mix !! (read i - 1)
