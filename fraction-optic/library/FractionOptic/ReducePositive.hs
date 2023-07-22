module FractionOptic.ReducePositive where

import Integer
  ( IntegerConvert (convert),
    IntegerNarrow (narrow),
    Natural,
    Positive,
  )

reducePositive ::
  (Positive, Positive) ->
  (Positive, Positive)
reducePositive (x, y) =
  let gcf = greatestCommonFactor (x, y)
   in (quot x gcf, quot y gcf)

greatestCommonFactor :: (Positive, Positive) -> Positive
greatestCommonFactor (a, b) =
  case narrow (remainder a b) of
    Nothing -> b
    Just c -> greatestCommonFactor (b, c)

remainder :: Positive -> Positive -> Natural
remainder a b = rem (convert a) (convert b)
