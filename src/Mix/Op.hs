module Mix.Op
    ( loadRegister
    , storeRegister
    , storeZero
    ) where

import Mix.Model
import Data.Array


-- LD{A,X}: Loading always zeroes out the register.
loadRegister :: Mix -> Instruction -> Mix
loadRegister mix Instruction{address, fSpec} = mix {
  rA = copyCell fSpec (memory mix ! address) newCell
}


-- ST{A,X}
storeRegister :: Mix -> Instruction -> Mix
storeRegister mix Instruction{address, fSpec} = mix {
  memory = memory mix // [(address, copyCell fSpec (rA mix) (memory mix ! address))]
}


-- STZ
storeZero :: Mix -> Instruction -> Mix
storeZero mix Instruction{address, fSpec} = mix {
  memory = memory mix // [(address, copyCell fSpec newCell (memory mix ! address))]
}
