module Mix.Assembly
    ( parseLine
    ) where

import Data.Array
import Data.String.Interpolate
import qualified Data.List.Split as Split
import qualified Data.Text as Text
import qualified Data.Char as Char (isSpace)

import Mix.Model


isComment :: String -> Bool
isComment = Text.isPrefixOf "*" . Text.strip . Text.pack


isBlank :: String -> Bool
isBlank = all Char.isSpace


-- Returns a pair containing the opcode and the register it affects (if any).
parseOp :: String -> (Op, Maybe String)
parseOp ('L':'D':cs)      = (Load, Just cs)
parseOp ('S':'T':'Z':cs)  = (Zero, Nothing)
parseOp ('S':'T':cs)      = (Store, Just cs)
parseOp ('A':'D':'D':cs)  = (Add, Nothing)


parseLine :: String -> Instruction
parseLine line
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
