module Mix.Parser
    ( parseLine
    ) where

import Data.Array
import Data.String.Interpolate
import Text.Regex.Posix ((=~))

import qualified Data.List.Split as Split
import qualified Data.Char as Char (isSpace)

import Mix.Data
import Mix.Data.Display


-- OP ADDRESS,I(F) => OP ADDRESS I (Fl:Fh)
asmPattern :: String
asmPattern = "[[:space:]]*([[:alnum:]]+)[[:space:]]+([-])?([[:digit:]]+)([,][[:digit:]])?([(][[:digit:]][:][[:digit:]][)])?"


isComment :: String -> Bool
isComment = Text.isPrefixOf "*" . Text.strip . Text.pack


isBlank :: String -> Bool
isBlank = all Char.isSpace


-- Returns a pair containing the opcode and the register it affects (if any).
parseOp :: String -> (Op, Maybe String)
parseOp ('L':'D':cs)      = (Load, Just cs)
parseOp ('S':'T':'Z':cs)  = (Zero, Nothing)
parseOp ('S':'T':cs)      = (Store, Just cs)
parseOp ('I':'N':'C':cs)  = (Increment, Just cs)
parseOp ('S':'E':'T':cs)  = (Set, Just cs)
parseOp ('A':'D':'D':cs)  = (Add, Just cs)
parseOp x                 = fail ("Parse error: " ++ x) (Zero, Nothing) -- TODO make this an Either


parseLine :: String -> Instruction
parseLine line
    | isComment line  = Comment
    | isBlank line    = Blank
    | otherwise       = Instruction {
        op      = fst $ parseOp $ operands !! 1
      , target  = snd $ parseOp $ operands !! 1
      , sign    = operandAt 2 Plus
      , address = operandAt 3 -1
      , iSpec   = operandAt 4 0
      , fSpec   = operandAt 5 (FieldSpec 0 5)
    }
  where
    operands                = head (line =~ asmPattern :: [[String]])
    operandAt i def
        | length operands > i = read (operands !! i)
        | otherwise           = def
