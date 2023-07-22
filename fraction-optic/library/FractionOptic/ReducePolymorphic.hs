module FractionOptic.ReducePolymorphic where

import FractionOptic.NonzeroMagnitude
  ( NonzeroMagnitudeAffineTraversal (nonzeroMagnitude),
  )
import FractionOptic.ReducePositive (reducePositive)
import Integer (Positive)
import Optics.Core (preview, set)

reduce :: NonzeroMagnitudeAffineTraversal a => (a, Positive) -> (a, Positive)
reduce (x, y) =
  case preview nonzeroMagnitude x of
    Nothing -> (x, 1)
    Just xMagnitude -> (xReduced, yReduced)
      where
        (xMagnitudeReduced, yReduced) =
          reducePositive (xMagnitude, y)

        xReduced =
          set nonzeroMagnitude xMagnitudeReduced x
