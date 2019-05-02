module Mix
    ( executeMix
    ) where

import System.IO
import Data.String.Interpolate

import Mix.Assembly
import Mix.Model
import Mix.Op


executeMix :: String -> IO ()
executeMix mixFile = do
    handle  <- openFile mixFile ReadMode
    text    <- hGetContents handle
    let result = executeProgram newMix text
    putStrLn result

    hClose handle


-- Runs a MIX program and returns the MIX computer's final state.
executeProgram :: Mix -> String -> String
executeProgram mix code = show $ executeLine (mix{ source = lines code }) (lines code)


-- executes each line of the mix program, passing the output as a new computer to the next
executeLine :: Mix -> [String] -> Mix
executeLine mix []      = mix
executeLine mix (l:ls)  = executeLine (executeOp mix $ parseLine l) ls


executeOp :: Mix -> Instruction -> Mix
executeOp mix Instruction{op, target, address}
    | op == Load  = loadRegister mix target address
    | op == Store = storeRegister mix target address
    | op == Zero  = storeZero mix address
    | otherwise   = mix -- TODO make this an Either

executeOp mix _   = mix
