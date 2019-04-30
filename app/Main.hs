module Main where

import Text.RawString.QQ
import Mix

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
    executeMix "a.mix"
