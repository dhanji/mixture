module Mix.Op
  ( loadRegister
  , setRegister
  , storeRegister
  , storeZero
  , addAccumulator
  ) where

import Mix.Data
import Mix.Data.Cell
import Data.Array


-- Made up instruction for testing only.
setRegister :: Mix -> Instruction -> Mix
setRegister mix Instruction{address, target, sign} = set mix target Cell {
    sign  = sign
  , bytes = bytes (toCell address)
}


-- LD{A,X}: Loading always zeroes out the register.
loadRegister :: Mix -> Instruction -> Mix
loadRegister mix Instruction{address, target, fSpec} = set mix target value
  where
    value = select fSpec (memory mix ! address)


-- ADD: Loads from memory and adds to the A register.
addAccumulator :: Mix -> Instruction -> Mix
addAccumulator mix Instruction{address, target, fSpec} = set mix target result
  where
    register = get mix target
    value    = select fSpec (memory mix ! address)
    result   = addCells register value


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
