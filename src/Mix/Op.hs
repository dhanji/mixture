module Mix.Op where

import Mix.Model
import Data.Array


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
