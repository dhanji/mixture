module Main where

import System.Environment
import Text.RawString.QQ

import Mix


banner :: String
banner = [r|
                  .__          __
            _____ |__|__  ____/  |_ __ _________   ____
           /     \|  \  \/  /\   __\  |  \_  __ \_/ __ \
          |  Y Y  \  |>    <  |  | |  |  /|  | \/\  ___/
          |__|_|  /__/__/\_ \ |__| |____/ |__|    \___  >
                \/         \/                         \/   1.0

          a MIX computer in λaskell |]


main :: IO ()
main = do
    putStrLn banner
    args <- getArgs

    case parse args of
      Left msg    -> putStrLn msg
      Right file  -> executeMix file


parse :: [String] -> Either String String
parse [arg] = Right arg
parse _     = Left "\nusage: mixture <file.mix>"
