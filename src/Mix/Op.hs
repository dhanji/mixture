module Mix.Op
  ( loadRegister
  , setRegister
  , storeRegister
  , storeZero
  ) where

import Mix.Model
import Data.Array


-- Made up instruction for testing only.
setRegister :: Mix -> Instruction -> Mix
setRegister mix Instruction{address, target, sign} = set mix target Cell {
    sign  = sign
  , bytes = toBytes address
}


-- LD{A,X}: Loading always zeroes out the register.
loadRegister :: Mix -> Instruction -> Mix
loadRegister mix Instruction{address, target, fSpec} = set mix target value
  where
    value = rightCopy fSpec (memory mix ! address) newCell


-- ST{A,X}
storeRegister :: Mix -> Instruction -> Mix
storeRegister mix Instruction{address, target, fSpec} = mix {
  memory = memory mix // [(address, value)]
}
  where
    register  = get mix target
    value     = leftCopy fSpec register (memory mix ! address)


-- STZ
storeZero :: Mix -> Instruction -> Mix
storeZero mix Instruction{address, fSpec} = mix {
  memory = memory mix // [(address, value)]
}
  where
    value = leftCopy fSpec newCell (memory mix ! address)
