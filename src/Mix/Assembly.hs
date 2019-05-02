module Mix.Assembly
    ( parseLine
    ) where

import Data.Array
import Data.String.Interpolate
import Text.Regex.Posix ((=~))

import qualified Data.List.Split as Split
import qualified Data.Text as Text
import qualified Data.Char as Char (isSpace)

import Mix.Model

import Debug.Trace (trace) -- TODO delete

-- OP ADDRESS,I(F) => OP ADDRESS I (Fl:Fh)
asmPattern :: String
asmPattern = "[[:space:]]*([[:alnum:]]+)[[:space:]]+([[:digit:]]+)([,][[:digit:]])?([(][[:digit:]][:][[:digit:]][)])?"


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
parseOp x                 = fail ("Parse error: " ++ x) (Add, Nothing) -- TODO don't fail!


parseLine :: String -> Instruction
parseLine line
    | isComment line  = Comment
    | isBlank line    = Blank
    | otherwise       = Instruction {
        op      = fst $ parseOp $ operands !! 1
      , target  = snd $ parseOp $ operands !! 1
      , address = operandAt 2 -1
      , iSpec   = operandAt 3 0
      , fSpec   = operandAt 4 (FieldSpec 0 5)
    }
  where
    operands                = head (line =~ asmPattern :: [[String]])
    operandAt i def
      | length operands > i = read (operands !! i)
      | otherwise           = def
