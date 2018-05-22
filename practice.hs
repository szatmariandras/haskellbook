{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}

import Test.QuickCheck
import Test.QuickCheck.Function


functorIdentity :: (Functor f, Eq (f a)) =>
                     f a
                  -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Functor f, Eq (f c)) =>
                     f a
                  -> Fun a b
                  -> Fun b c
                  -> Bool
functorCompose x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)

newtype Identity a = Identity a deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity $ f a

-------------------------------------------------------

newtype Constant a b =
  Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Monoid a
        => Applicative (Constant a) where
  pure _ = Constant mempty
  Constant a <*> Constant b = Constant $ a `mappend` b


-------------------------------------------------------

data Pair a = Pair a a deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

-------------------------------------------------------

data Two a b  = Two a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

instance Functor (Two a) where
  fmap f (Two a b) = Two a $ f b

-------------------------------------------------------

data Three a b c = Three a b c deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
    Arbitrary (Three a b c)
  where
    arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b $ f c

-------------------------------------------------------

data Three' a b = Three' a b b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

-------------------------------------------------------

data Possibly a = LolNope | Yeppers a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Possibly a) where
  arbitrary = oneof [ return LolNope,
                      Yeppers <$> arbitrary
                    ]

instance Functor Possibly where
  fmap _ LolNope     = LolNope
  fmap f (Yeppers a) = Yeppers $ f a

-------------------------------------------------------

data Quant a b =
    Finance
  | Desk a
  | Bloor b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Quant a b) where
  arbitrary = oneof [ return Finance,
                      Desk <$> arbitrary,
                      Bloor <$> arbitrary
                    ]

instance Functor (Quant a) where
  fmap f (Bloor b)   = Bloor $ f b
  fmap _ (Desk a)    = Desk a
  fmap _ Finance     = Finance

-------------------------------------------------------

type Tested = Quant Int Int

type IntToInt = Fun Int Int

type TestedFC =
     Tested
  -> IntToInt
  -> IntToInt
  -> Bool

main :: IO ()
main = do
    quickCheck (functorCompose :: TestedFC)
    quickCheck (functorIdentity :: Tested -> Bool)
