import FractionOptic
import Integer
import Test.Hspec

main :: IO ()
main = hspec do
  it "" $ reduce @Natural (0, 1) `shouldBe` (0, 1)
  it "" $ reduce @Natural (0, 5) `shouldBe` (0, 1)
  it "" $ reduce @Natural (1, 5) `shouldBe` (1, 5)
  it "" $ reduce @Natural (4, 6) `shouldBe` (2, 3)
  it "" $ reduce @Natural (9, 6) `shouldBe` (3, 2)
  it "" $ reduce @Natural (18, 9) `shouldBe` (2, 1)

  it "" $ reduce @Positive (1, 5) `shouldBe` (1, 5)
  it "" $ reduce @Positive (4, 6) `shouldBe` (2, 3)
  it "" $ reduce @Positive (9, 6) `shouldBe` (3, 2)
  it "" $ reduce @Positive (18, 9) `shouldBe` (2, 1)

  it "" $ reduce @Integer (-1, 1) `shouldBe` (-1, 1)
  it "" $ reduce @Integer (-5, 1) `shouldBe` (-5, 1)
  it "" $ reduce @Integer (-4, 6) `shouldBe` (-2, 3)
  it "" $ reduce @Integer (-9, 6) `shouldBe` (-3, 2)
  it "" $ reduce @Integer (0, 1) `shouldBe` (0, 1)
  it "" $ reduce @Integer (0, 5) `shouldBe` (0, 1)
  it "" $ reduce @Integer (1, 5) `shouldBe` (1, 5)
  it "" $ reduce @Integer (4, 6) `shouldBe` (2, 3)
  it "" $ reduce @Integer (9, 6) `shouldBe` (3, 2)
  it "" $ reduce @Integer (18, 9) `shouldBe` (2, 1)

  it "" $ reduce @Signed (-1, 1) `shouldBe` (-1, 1)
  it "" $ reduce @Signed (-5, 1) `shouldBe` (-5, 1)
  it "" $ reduce @Signed (-4, 6) `shouldBe` (-2, 3)
  it "" $ reduce @Signed (-9, 6) `shouldBe` (-3, 2)
  it "" $ reduce @Signed (0, 1) `shouldBe` (0, 1)
  it "" $ reduce @Signed (0, 5) `shouldBe` (0, 1)
  it "" $ reduce @Signed (1, 5) `shouldBe` (1, 5)
  it "" $ reduce @Signed (4, 6) `shouldBe` (2, 3)
  it "" $ reduce @Signed (9, 6) `shouldBe` (3, 2)
  it "" $ reduce @Signed (18, 9) `shouldBe` (2, 1)
