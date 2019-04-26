module Mix.Machine where

import Data.Array
import Data.String.Interpolate
import qualified Data.List.Split as Split
import qualified Data.Text as Text
import qualified Data.Char as Char (isSpace)

import Mix.Model


newMix :: Mix
newMix = Mix {
    rA = newCell,
    rX = newCell,
    memory = newMemory
  }
  where
    -- Mix computers have 4000 words of memory.
    newMemory   = array (1, 4000) []


-- Returns a pair containing the opcode and the register it affects
parseOp :: String -> (Op, String)
parseOp ('L':'D':cs)  = (Load, cs)
parseOp ('S':'T':cs)  = (Store, cs)


parseInstruction :: String -> Instruction
parseInstruction line
    | isComment line  = Comment
    | isBlank line    = Blank
    | otherwise       = Instruction {
    op      = fst $ parseOp opcode
  , target  = snd $ parseOp opcode

  , address = parseAddress (words line !! 1)
  , iSpec   = 0
  , fSpec   = FieldSpec 0 5
}
  where
    opcode                = head (words line)
    parseAddress operand  = read (head $ Split.splitOn "," operand) :: Int


isComment :: String -> Bool
isComment = Text.isPrefixOf "*" . Text.strip . Text.pack


isBlank :: String -> Bool
isBlank = all Char.isSpace
