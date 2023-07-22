module FractionOptic where

import Integer
import Optics.Core


---  Monomorphic fraction reduction  ---

reducePositive ::
  (Positive, Positive) ->
  (Positive, Positive)
reducePositive (x, y) =
  let gcf = greatestCommonFactor (x, y)
   in (quot x gcf, quot y gcf)


---  The class  ---

class NonzeroMagnitudeAffineTraversal a where
  nonzeroMagnitude :: AffineTraversal' a Positive


---  Polymorphic fraction reduction  ---

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

greatestCommonFactor :: (Positive, Positive) -> Positive
greatestCommonFactor (a, b) =
  case narrow (remainder a b) of
    Nothing -> b
    Just c -> greatestCommonFactor (b, c)

remainder :: Positive -> Positive -> Natural
remainder a b = rem (convert a) (convert b)


---  Instances  ---

instance NonzeroMagnitudeAffineTraversal Positive where
  nonzeroMagnitude = castOptic simple

instance NonzeroMagnitudeAffineTraversal Natural where
  nonzeroMagnitude = castOptic (prism' convert narrow)

instance NonzeroMagnitudeAffineTraversal Integer where
  nonzeroMagnitude = iso convert convert % nonzeroMagnitude @Signed

instance NonzeroMagnitudeAffineTraversal Signed where
  nonzeroMagnitude = nonzero % _2

nonzero :: Prism' Signed (Sign, Positive)
nonzero = prism' nonzeroReview nonzeroPreview

nonzeroReview :: (Sign, Positive) -> Signed
nonzeroReview (s, p) = NonZero s p

nonzeroPreview :: Signed -> Maybe (Sign, Positive)
nonzeroPreview = \case Zero -> Nothing; NonZero s p -> Just (s, p)
