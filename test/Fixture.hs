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


mulByOne :: Specification
mulByOne = ("with '1000' multiplies (2:2), i.e. 1x", [i|
  SETA    1000
  STA     2000
  MUL     2000(2:2)
|], 1000)


mulByZeroes :: Specification
mulByZeroes = ("with '1000' multiplies (3:4), i.e. 0x", [i|
  SETA    1000
  STA     2000
  MUL     2000(3:4)
|], 0)


mulByNegativeOne :: Specification
mulByNegativeOne = ("with '-1000' multiplies (0:2) flips sign", [i|
  SETA    -1000
  STA     2000
  MUL     2000(0:2)
|], 1000)


divByTwo :: Specification
divByTwo = ("with '20000' divides (1:1), i.e. /2", [i|
  SETA    20000
  STA     2000
  DIV     2000(1:1)
|], 10000)


divBySelf :: Specification
divBySelf = ("with '20000' divides (0:5), i.e. ==1", [i|
  SETA    20000
  STA     2000
  DIV     2000
|], 1)


divByNegativeOne :: Specification
divByNegativeOne = ("with '-16789' divides (0:1) flips sign", [i|
  SETA    -16789
  STA     2000
  DIV     2000(0:1)
|], 16789)


addDoublesCellNoFieldSpec :: Specification
addDoublesCellNoFieldSpec = ("with '2345' adds itself, i.e. 2x", [i|
  SETA    2345
  STA     2000

  * test instructon (should set A to 2345 x2)
  ADD     2000
|], 2345 * 2)
