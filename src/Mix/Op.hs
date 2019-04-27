module Mix.Op where

import Mix.Model
import Data.Array


loadRegister :: Mix -> String -> Int -> Mix
loadRegister mix target address = mix {
  rA     = memory mix ! address
}


storeRegister :: Mix -> String -> Int -> Mix
storeRegister mix target address = mix {
  memory = memory mix // [(address, rA mix)]
}
