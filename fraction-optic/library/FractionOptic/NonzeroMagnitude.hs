module FractionOptic.NonzeroMagnitude where

import Integer
  ( IntegerConvert (convert),
    IntegerNarrow (narrow),
    Natural,
    Positive,
    Sign,
    Signed (NonZero, Zero),
  )
import Optics.Core
  ( AffineTraversal',
    Field2 (_2),
    Prism',
    castOptic,
    iso,
    prism',
    simple,
    (%),
  )

class NonzeroMagnitudeAffineTraversal a where
  nonzeroMagnitude :: AffineTraversal' a Positive

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
