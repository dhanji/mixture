module Mix.Data.Display where

import Data.Char
import Data.String.Interpolate

import Mix.Data


{- Package collects show / read instances for Mix types -}

instance Show Sign where
  show Plus   = "+"
  show Minus  = "-"

instance Read Sign where
  readsPrec _ "-" = [(Minus, "")]
  readsPrec _ _   = [(Plus, "")]

instance Show Cell where
  show cell = [i|#{sign (cell :: Cell)}#{toBitString $ bytes cell}|]
    where
      toBitString = foldl (++) "" . map show

instance Read FieldSpec where
  readsPrec _ ('(':l:':':h:")") =
    let low = ord l - ord '0'
        hi  = ord h - ord '0' in
      [(FieldSpec low hi, "")]
  readsPrec _ _                 = [(FieldSpec 0 5, "")]

-- Purely for symmetry with the Read instance
instance Show FieldSpec where
  show (FieldSpec low hi)  = [i|(#{low}:#{hi})|]

instance Show Mix where
  show mix = [i|

      register A              register X
      #{show $ rA mix}                 #{show $ rX mix}

  I registers:
  #{map show (rI mix)}

  register J:
  #{show $ rJ mix}


assembly:
|] ++ (foldl (++) "" [line ++ "\n" | line <- source mix])
