module Lib
    ( executeMix
    ) where

import System.IO
import Data.Array
import Data.String.Interpolate
import qualified Data.Word as DW hiding (Word)
import qualified Data.Text as Text

import Mix.Machine
import Mix.Model


executeMix :: String -> IO ()
executeMix mixFile = do
    handle  <- openFile mixFile ReadMode
    text    <- hGetContents handle
    let result = executeProgram newMix text
    putStrLn result

    hClose handle


-- Runs a MIX program and returns the MIX computer's final state.
executeProgram :: Mix -> String -> String
executeProgram mix code = show $ executeLine mix (lines code)


-- executes each line of the mix program, passing the output as a new computer to the next
executeLine :: Mix -> [String] -> Mix
executeLine mix []      = mix
executeLine mix (l:ls)  = executeLine (executeOp mix $ parseInstruction l) ls


executeOp :: Mix -> Instruction -> Mix
executeOp mix Instruction{op, target, address}
    | op == Load  = loadRegister mix target address
    | op == Store = storeRegister mix target address
    | otherwise   = mix -- TODO make this an Either

executeOp mix _   = mix


loadRegister :: Mix -> String -> Int -> Mix
loadRegister mix target address = Mix {
    rA     = memory mix ! address

  , rX     = rX mix
  , memory = memory mix
}


storeRegister :: Mix -> String -> Int -> Mix
storeRegister mix target address = Mix {
    rA     = rA mix
  , rX     = rX mix

  , memory = memory mix // [(address, rA mix)]
}
