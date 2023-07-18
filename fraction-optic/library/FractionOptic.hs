module FractionOptic where

import Integer
import Optics.Core

class NonzeroMagnitudeAffineTraversal a where
  nonzeroMagnitude :: AffineTraversal' a Positive

instance NonzeroMagnitudeAffineTraversal Positive where
  nonzeroMagnitude = castOptic simple

instance NonzeroMagnitudeAffineTraversal Natural where
  nonzeroMagnitude = castOptic (prism' convert narrow)

instance NonzeroMagnitudeAffineTraversal Signed where
  nonzeroMagnitude = nonzero % _2

nonzero :: Prism' Signed (Sign, Positive)
nonzero = prism' nonzeroReview nonzeroPreview

nonzeroReview :: (Sign, Positive) -> Signed
nonzeroReview (s, p) = NonZero s p

nonzeroPreview :: Signed -> Maybe (Sign, Positive)
nonzeroPreview = \case Zero -> Nothing; NonZero s p -> Just (s, p)

instance NonzeroMagnitudeAffineTraversal Integer where
  nonzeroMagnitude = iso convert convert % nonzeroMagnitude @Signed

reduce :: NonzeroMagnitudeAffineTraversal a => (a, Positive) -> (a, Positive)
reduce (x, y) =
  case preview nonzeroMagnitude x of
    Nothing -> (x, 1)
    Just m ->
      let d = greatestCommonFactor m y
      in (set nonzeroMagnitude (quot m d) x, quot y d)

remainder :: Positive -> Positive -> Natural
remainder a b = rem (convert a) (convert b)

greatestCommonFactor :: Positive -> Positive -> Positive
greatestCommonFactor a b =
  case narrow (remainder a b) of
    Nothing -> b
    Just c -> greatestCommonFactor b c
