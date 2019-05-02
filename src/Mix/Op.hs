module Mix.Op
    ( loadRegister
    , storeRegister
    , storeZero
    ) where

import Mix.Model
import Data.Array


-- Reads a memory location, honoring the I- and F- spec given.
readMemory :: Mix -> Instruction -> Cell
readMemory mix Instruction{target, address, fSpec} = memory mix ! address


-- LD{A,X}
loadRegister :: Mix -> Instruction -> Mix
loadRegister mix instruction = mix {
  rA     = readMemory mix instruction
}


-- ST{A,X}
storeRegister :: Mix -> Instruction -> Mix
storeRegister mix instruction = mix {
  memory = memory mix // [((address instruction), rA mix)]
}


-- STZ
storeZero :: Mix -> Instruction -> Mix
storeZero mix instruction = mix {
  memory = memory mix // [((address instruction), newCell)]
}
