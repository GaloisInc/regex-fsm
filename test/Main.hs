{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Debug.Trace
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Regex.Types
import Regex.Parse
import Regex.ENFA
import Regex.DFA
import Regex.MBP

instance Arbitrary a => Arbitrary (Reg a) where
  arbitrary =
    oneof [ Union <$> arbitrary <*> arbitrary
          , Cat <$> arbitrary <*> arbitrary
          , Rep <$> arbitrary
          , Lit <$> arbitrary
          , pure Eps
          ]

main :: IO ()
main =
  hspec $ do
    describe "regex-fsm tests" $ do
      it "DFA and Minimized DFA are the same" $
         property $ \(regex :: Reg Char) ->
            subset (thompsons regex) == minimize (subset (thompsons regex))



