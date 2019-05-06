module Mix
    ( executeMix
    ) where

import System.IO
import Data.String.Interpolate

import Mix.Parser
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
executeLine mix (l:ls)  = case parseLine l of
    Blank       -> executeLine mix ls
    Comment     -> executeLine mix ls
    instruction -> executeLine (executeOp mix instruction) ls


executeOp :: Mix -> Instruction -> Mix
executeOp mix instruction = case op instruction of
    Set     -> setRegister mix instruction
    Load    -> loadRegister mix instruction
    Store   -> storeRegister mix instruction
    Zero    -> storeZero mix instruction
    _       -> mix -- TODO make this an Either
