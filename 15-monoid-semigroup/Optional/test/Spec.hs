import Test.Hspec
import Test.QuickCheck
import Optional

import Data.Monoid

type MonoidOptional = Optional String -> Optional String -> Optional String -> Bool
type MonoidId = Optional String -> Bool

type FirstOptional = First' String -> First' String -> First' String -> Bool
type FirstId = First' String -> Bool

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = do
    x <- arbitrary
    oneof [return Nada, return (Only x)]

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = do
    x <- arbitrary
    return $ First' x

mappendIsAssociative :: (Eq m, Monoid m) => m -> m -> m -> Bool
mappendIsAssociative a b c = (a <> (b <> c)) == ((a <> b) <> c)

mappendIsLeftAssociative :: (Eq m, Monoid m) => m -> Bool
mappendIsLeftAssociative a = (mempty <> a) == a

mappendIsRightAssociative :: (Eq m, Monoid m) => m -> Bool
mappendIsRightAssociative a = (a <> mempty) == a

main :: IO ()
main = hspec $ do
  describe "Optional with Monoidal type argument" $ do

    describe "mempty" $ do
      it "returns Nada for mempty" $ do
        (mempty :: Optional (Sum Int)) `shouldBe` Nada

    describe "mappend" $ do
      it "returns Only (a `mappend` b) for mappend (Only a) (Only b)" $ do
        mappend (Only (Sum 1)) (Only (Sum 2)) `shouldBe` Only (Sum 3)
      it "returns the 'Only' argument if one argument is Nada" $ do
        mappend (Only (Sum 1)) Nada `shouldBe` Only (Sum 1)
        mappend Nada (Only (Sum 1)) `shouldBe` Only (Sum 1)
      it "obeys identity law" $ do
        property (mappendIsAssociative :: MonoidOptional)
      it "obeys left identity law" $ do
        property (mappendIsLeftAssociative :: MonoidId)
      it "obeys right identity law" $ do
        property (mappendIsRightAssociative :: MonoidId)

  describe "Monoid (First' a)" $ do

    describe "mappend" $ do
      it "returns first Only if both arguments are Only" $ do
        mappend (First' $ Only (Sum 1)) (First' $ Only (Sum 2)) `shouldBe` (First' $ Only (Sum 1))
      it "returns the 'Only' argument if one argument is Nada" $ do
        mappend (First' $ Only (Sum 1)) (First' Nada) `shouldBe` (First' $ Only (Sum 1))
        mappend (First' Nada) (First' $ Only (Sum 1)) `shouldBe` (First' $ Only (Sum 1))
      it "obeys identity law" $ do
        property (mappendIsAssociative :: FirstOptional)
      it "obeys left identity law" $ do
        property (mappendIsLeftAssociative :: FirstId)
      it "obeys right identity law" $ do
        property (mappendIsRightAssociative :: FirstId)
