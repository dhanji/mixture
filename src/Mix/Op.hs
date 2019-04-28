module Mix.Op where

import Mix.Model
import Data.Array


-- LD{A,X}
loadRegister :: Mix -> Maybe String -> Int -> Mix
loadRegister mix target address = mix {
  rA     = memory mix ! address
}


-- ST{A,X}
storeRegister :: Mix -> Maybe String -> Int -> Mix
storeRegister mix target address = mix {
  memory = memory mix // [(address, rA mix)]
}


-- STZ
storeZero :: Mix -> Int -> Mix
storeZero mix address = mix {
  memory = memory mix // [(address, newCell)]
}
