module Fixture where

import Data.String.Interpolate

type Specification = (String, String, Int)


subFieldParts :: Specification
subFieldParts = ("in '2345' subtracts 2000(4:4) from 2000(0:1)", [i|
  SETA    2345
  STA     2000

  * test instructon (should set A to 2345 - 4)
  SUB     2000(4:4)
|], 2341)


addFieldParts :: Specification
addFieldParts = ("in '2345' adds 2000(4:4) to 2000(0:1)", [i|
  SETA    2345
  STA     2000

  * test instructon (should set A to 2345 + 4)
  ADD     2000(4:4)
|], 2349)
