import Test.Hspec
import Test.QuickCheck
import ChapterExercises

import Data.Semigroup

semigroupAssociative :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssociative a b c = (a <> (b <> c)) == ((a <> b) <> c)

-- Trivial
type TrivialAssociative = Trivial -> Trivial -> Trivial -> Bool

instance Arbitrary Trivial where
  arbitrary = return Trivial

-- Identity a
type IdentityAssociative = Identity [Integer]
                        -> Identity [Integer]
                        -> Identity [Integer]
                        -> Bool

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

-- Two a b
type TwoAssociative = Two ([Integer]) ([Float])
                   -> Two ([Integer]) ([Float])
                   -> Two ([Integer]) ([Float])
                   -> Bool

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

-- Three a b c
type ThreeAssociative = Three ([Integer]) ([Float]) ([Integer])
                     -> Three ([Integer]) ([Float]) ([Integer])
                     -> Three ([Integer]) ([Float]) ([Integer])
                     -> Bool

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
  Arbitrary (Three a b c) where
    arbitrary = do
      Two a b <- arbitrary
      c <- arbitrary
      return $ Three a b c

-- Four a b c
type FourAssociative = Four ([Integer]) ([Float]) ([Integer]) ([Float])
                    -> Four ([Integer]) ([Float]) ([Integer]) ([Float])
                    -> Four ([Integer]) ([Float]) ([Integer]) ([Float])
                    -> Bool

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
  Arbitrary (Four a b c d) where
    arbitrary = do
      Two a b <- arbitrary
      Two c d<- arbitrary
      return $ Four a b c d

-- BoolConj
type BoolConjAssociative = BoolConj -> BoolConj -> BoolConj -> Bool

instance Arbitrary (BoolConj) where
  arbitrary = do
    a <- arbitrary
    return $ BoolConj a

-- BoolDisj
type BoolDisjAssociative = BoolDisj -> BoolDisj -> BoolDisj -> Bool

instance Arbitrary (BoolDisj) where
  arbitrary = do
    a <- arbitrary
    return $ BoolDisj a

-- Or a b
type OrAssociative = Or Int Int -> Or Int Int -> Or Int Int -> Bool

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    (a, b) <- arbitrary
    oneof [return $ Fst a, return $ Snd b]

-- Combine a b
type CombineAssociative = Combine Int (Sum Int)
                       -> Combine Int (Sum Int)
                       -> Combine Int (Sum Int)
                       -> Bool

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
    f <- arbitrary
    return $ Combine f

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = do
    f <- arbitrary
    return $ Comp f

type ValidationAssociative = Validation Int Int
                          -> Validation Int Int
                          -> Validation Int Int
                          -> Bool

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    (a, b) <- arbitrary
    oneof [return $ Failure' a, return $ Success' b]


-- Validation a b
main :: IO ()
main = hspec $ do
  describe "Semigroup <>" $ do
    it "is Associative for Trivial" $ do
      property (semigroupAssociative :: TrivialAssociative)
    it "is Associative for (Identity a)" $ do
      property (semigroupAssociative :: IdentityAssociative)
    it "is Associative for (Two a b)" $ do
      property (semigroupAssociative :: TwoAssociative)
    it "is Associative for (Three a b c)" $ do
      property (semigroupAssociative :: ThreeAssociative)
    it "is Associative for (Four a b c d)" $ do
      property (semigroupAssociative :: FourAssociative)
    it "is Associative for BoolConj" $ do
      property (semigroupAssociative :: BoolConjAssociative)
    it "is Associative for BoolDisj" $ do
      property (semigroupAssociative :: BoolDisjAssociative)
    it "is Associative for (Or a b)" $ do
      property (semigroupAssociative :: OrAssociative)
    it "is Associative for (Validation a b)" $ do
      property (semigroupAssociative :: ValidationAssociative)

  describe "Mem" $ do
    it "combines functions" $ do
      let f' = Mem $ \x -> ("hi", x + 1)

      runMem (f' <> mempty) 0 `shouldBe` ("hi", 1)
      runMem (mempty <> f') 0 `shouldBe` ("hi", 1)
      (runMem mempty 0 :: (String, Int)) `shouldBe` ("", 0) runMem (f' <> mempty) 0 == runMem f' 0 `shouldBe` True
      runMem (mempty <> f') 0 == runMem f' 0 `shouldBe` True
