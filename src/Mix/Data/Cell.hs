module Mix.Data.Cell where

import Data.Array
import Mix.Data


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


-- Returns a slice of the given cell according to the FieldSpec
-- Contents of source are copied to a new cell such that bits
-- in the result and shifted to the right and left-padded with zeros.
select :: FieldSpec -> Cell -> Cell
select (FieldSpec low hi) Cell{sign,bytes} = Cell {
    sign  = if | low == 0   -> sign
               | otherwise  -> Plus
  , bytes = zeroes ++ suffix bytes
}
  where
    from    = low - 1
    zeroes  = replicate (cellWidth - length (suffix bytes)) 0
    suffix
        | low == 0  = take (hi - from - 1)
        | otherwise = take (hi - from) . drop from


addCells :: Cell -> Cell -> Cell
addCells left right = toCell $ toInt left + toInt right


-- Returns a slice of the given bitstring according to the FieldSpec
-- Contents of source are copied to a new cell such that
-- any bits of target that are not covered by the F-spec
-- are "preserved" intact in the result and shifted to the left.
leftCopy :: FieldSpec -> Cell -> Cell -> Cell
leftCopy (FieldSpec low hi) Cell{sign=ssign,bytes=sbytes} Cell{sign=tsign,bytes=tbytes} = Cell {
    sign  = if | low == 0   -> ssign
               | otherwise  -> tsign
  , bytes = if | low == 0   -> keep hi sbytes ++ drop hi tbytes
               | otherwise  -> take from tbytes ++ keep size sbytes ++ drop (from + size) tbytes
}
  where
    from      = low - 1
    size      = 1 + hi - low
    keep n bs = drop (length bs - n) bs


-- Converts an Int to a cellWidth-word Byte string.
toCell :: Int -> Cell
toCell i
    | i == 0    = Cell Plus (replicate cellWidth 0)
    | i > 0     = Cell Plus (bytize i)
    | otherwise = Cell Minus (bytize (abs i))
  where
    bytize i = [(i `mod` 10^n) `div` 10^(n - 1) | n <- [cellWidth,cellWidth - 1..1]]


-- Converts a Byte string into an Int
toInt :: Cell -> Int
toInt Cell{sign,bytes} = case sign of
    Plus  -> result
    Minus -> negate result
  where
    result    = sum [x * 10^n | (x, n) <- zip bytes exponents]
    exponents = reverse [0..length bytes - 1]


-- Destructively sets the value of a register to the given cell.
set :: Mix -> Maybe String -> Cell -> Mix
set mix Nothing _       = mix
set mix (Just reg) cell = case reg of
    "X"      -> mix { rX = cell }
    "J"      -> mix { rJ = cell }
    ('I':i)  -> let ix = read i in mix { rI = replace ix }
    _        -> mix { rA = cell }
  where
    replace ix = [if j == ix then cell else v | (j, v) <- zip [1..cellWidth] (rI mix)]


-- Returns the value of the register identified by the given String
get :: Mix -> Maybe String -> Cell
get mix Nothing    = newCell
get mix (Just reg) = case reg of
    "X"      -> rX mix
    "J"      -> rJ mix
    ('I':i)  -> rI mix !! (read i - 1)
    _        -> rA mix
