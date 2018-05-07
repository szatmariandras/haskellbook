module Addition where

import Test.Hspec
import Test.QuickCheck

sayHello :: IO ()
sayHello = putStrLn "Hey"

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
         | n < d = (count, n)
         | otherwise =
              go (n - d) d (count + 1)


main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "15 divided by 3 is 5" $ do
      (dividedBy 15 3 :: (Integer, Integer)) `shouldBe` (5, 0)
    it "22 divided by 5 is\
        \ 4 remainder 2" $ do
      (dividedBy 22 5 :: (Integer, Integer)) `shouldBe` (4, 2)
    it "x + 1 is always\
      \ greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

