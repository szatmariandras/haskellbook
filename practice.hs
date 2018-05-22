import Control.Monad
import Data.Semigroup
import Test.QuickCheck

semigroupAssoc :: (Eq m, Semigroup m)
            => m -> m -> m -> Bool
semigroupAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

newtype Identity a = Identity a deriving (Show, Eq)

instance Semigroup a => Semigroup (Identity a) where
  Identity a <> Identity b = Identity (a <> b)

type IdAssoc a =
  Identity a
  -> Identity a
  -> Identity a
  -> Bool

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

------------------------------------------------------------------

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a1 b1) <> (Two a2 b2) = Two (a1 <> a2) (b1 <> b2)

type TwoAssoc a b =
  Two a b
  -> Two a b
  -> Two a b
  -> Bool

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary


-----------------------------------------------------------------

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  BoolConj a <> BoolConj b = BoolConj $ a && b

type BoolConjAssoc =
  BoolConj -> BoolConj -> BoolConj -> Bool

instance Arbitrary BoolConj where
  arbitrary = BoolConj <$> arbitrary

-------------------------------------------------------

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  BoolDisj a <> BoolDisj b = BoolDisj $ a || b

type BoolDisjAssoc =
  BoolDisj -> BoolDisj -> BoolDisj -> Bool

instance Arbitrary BoolDisj where
  arbitrary = BoolDisj <$> arbitrary

-------------------------------------------------------

data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
  Fst _ <> Snd s = Snd s
  Fst a <> _     = Fst a
  Snd a <> _     = Snd a

type OrAssoc a b = Or a b -> Or a b -> Or a b -> Bool

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = oneof [Fst <$> arbitrary, Snd <$> arbitrary]

----------------------------------------------------------------

newtype Combine a b = Combine { unCombine :: (a -> b) }


--instance Arbitrary a => Arbitrary (Optional (Foo a)) where
--  arbitrary =
--    frequency [ (1, return $ None)
--              , (8, (Some . Foo) <$> arbitrary)
--              ]

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: OrAssoc (Sum Int) String)
