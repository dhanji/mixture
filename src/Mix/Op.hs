module Mix.Op
    ( loadRegister
    , storeRegister
    , storeZero
    ) where

import Mix.Model
import Data.Array


-- LD{A,X}
loadRegister :: Mix -> Instruction -> Mix
loadRegister mix instruction = mix {
  rA     = memory mix ! (address instruction)
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
