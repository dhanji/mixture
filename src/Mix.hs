module Mix
    ( executeMix
    , executeProgram
    ) where

import System.IO
import Data.String.Interpolate

import Mix.Data
import Mix.Data.Cell
import Mix.Op
import Mix.Parser


executeMix :: String -> IO ()
executeMix mixFile = do
    handle  <- openFile mixFile ReadMode
    text    <- hGetContents handle
    let result = executeProgram newMix text
    print result

    hClose handle


-- Runs a MIX program and returns the MIX computer's final state.
executeProgram :: Mix -> String -> Mix
executeProgram mix code = executeLine (mix{ source = lines code }) (lines code)


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
    Add     -> addAccumulator mix instruction
    Sub     -> subtractAccumulator mix instruction
    Mul     -> multiplyAccumulator mix instruction
    Div     -> divideAccumulator mix instruction
    _       -> mix -- TODO make this an Either
