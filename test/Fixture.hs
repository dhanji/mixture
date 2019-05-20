module Fixture where

import Data.String.Interpolate

type Specification = (String, String, Int)


subtractFieldParts :: Specification
subtractFieldParts = ("with '2345' subtracts (4:4)", [i|
  SETA    2345
  STA     2000

  * test instructon (should set A to 2345 - 4)
  SUB     2000(4:4)
|], 2345 - 4)


addSingleFieldPart :: Specification
addSingleFieldPart = ("with '2345' adds (4:4)", [i|
  SETA    2345
  STA     2000

  * test instructon (should set A to 2345 + 4)
  ADD     2000(4:4)
|], 2345 + 4)


addDoublesCell :: Specification
addDoublesCell = ("with '2345' adds (0:5), i.e. 2x", [i|
  SETA    2345
  STA     2000

  * test instructon (should set A to 2345 x2)
  ADD     2000(0:5)
|], 2345 * 2)


mulDoublesCell :: Specification
mulDoublesCell = ("with '2345' multiplies (2:2), i.e. 2x", [i|
  SETA    2345
  STA     2000

  * test instructon (should set A to 2345 x2)
  MUL     2000(2:2)
|], 2345 * 2)


addDoublesCellNoFieldSpec :: Specification
addDoublesCellNoFieldSpec = ("with '2345' adds itself, i.e. 2x", [i|
  SETA    2345
  STA     2000

  * test instructon (should set A to 2345 x2)
  ADD     2000
|], 2345 * 2)
