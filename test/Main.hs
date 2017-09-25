{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Boltzmann.Data
import Data.Data
import Data.Set
import Test.Hspec
import Test.QuickCheck

import Regex

instance (Data a, Arbitrary a) => Arbitrary (Reg a) where
  arbitrary = sized $ generatorPWith [positiveInts]

positiveInts :: Alias Gen
positiveInts = alias $ \() ->
  getPositive <$> arbitrary :: Gen Int

alphabet :: String
alphabet = "01"

main :: IO ()
main =
  hspec $ do
    describe "regex-fsm tests" $ do
      it "Should successfully simulate (0|1) on an ENFA" $ do
        let Right regex = parseRegex "(0|1)"
            thompson = thompsons regex :: ENFA Int Char
        simulateENFA "0" thompson `shouldBe` True
        simulateENFA "1" thompson  `shouldBe` True
        simulateENFA "c" thompson `shouldBe` False
        simulateENFA "" thompson `shouldBe` False

      it "Should successfully simulate (0*1) on an ENFA" $ do
        let Right regex = parseRegex "(0*1)"
            thompson = thompsons regex :: ENFA Int Char
        simulateENFA "" thompson `shouldBe` False
        simulateENFA "1" thompson `shouldBe` True
        simulateENFA "01" thompson `shouldBe` True
        simulateENFA "11" thompson `shouldBe` False
        simulateENFA "000001" thompson `shouldBe` True

      it "Should successfully simulate (0*|1*) on an ENFA" $ do
        let Right regex = parseRegex "(0*|1*)"
            thompson = thompsons regex :: ENFA Int Char
        simulateENFA "" thompson `shouldBe` True
        simulateENFA "01" thompson `shouldBe` False
        simulateENFA "0" thompson `shouldBe` True
        simulateENFA "1" thompson `shouldBe` True
        simulateENFA (replicate 100 '0') thompson `shouldBe` True
        simulateENFA (replicate 100 '1') thompson `shouldBe` True

      it "Should successfully simulate (0|1) on a DFA" $ do
        let Right regex = parseRegex "(0|1)"
            dfa :: DFA (Set Int) Char = subset alphabet (thompsons regex)
        simulateDFA "0" dfa `shouldBe` True
        simulateDFA "1" dfa `shouldBe` True
        simulateDFA "c" dfa `shouldBe` False
        simulateDFA "" dfa `shouldBe` False

      it "Should successfully simulate (0*1) on a DFA" $ do
        let Right regex = parseRegex "(0*1)"
            dfa :: DFA (Set Int) Char = subset alphabet (thompsons regex)
        simulateDFA "" dfa `shouldBe` False
        simulateDFA "1" dfa `shouldBe` True
        simulateDFA "01" dfa `shouldBe` True
        simulateDFA "11" dfa `shouldBe` False
        simulateDFA "0000001" dfa `shouldBe` True

      it "Should successfully simulate (0*|1*) on a DFA" $ do
        let Right regex = parseRegex "(0*|1*)"
            dfa :: DFA (Set Int) Char = subset alphabet (thompsons regex)
        simulateDFA "" dfa `shouldBe` True
        simulateDFA "01" dfa `shouldBe` False
        simulateDFA "0" dfa `shouldBe` True
        simulateDFA "0" dfa `shouldBe` True
        simulateDFA (replicate 100 '0') dfa `shouldBe` True
        simulateDFA (replicate 100 '1') dfa `shouldBe` True

      it "Should successfully simulate (0|1) on a minimized DFA" $ do
        let Right regex = parseRegex "(0|1)"
            dfa :: DFA (Set Int) Char = minimize $ subset alphabet (thompsons regex)
        simulateDFA "0" dfa `shouldBe` True
        simulateDFA "1" dfa `shouldBe` True
        simulateDFA "c" dfa `shouldBe` False
        simulateDFA "" dfa `shouldBe` False

      it "Should successfully simulate (0*1) on a minimized DFA" $ do
        let Right regex = parseRegex "(0*1)"
            dfa :: DFA (Set Int) Char = minimize $ subset alphabet (thompsons regex)
        simulateDFA "" dfa `shouldBe` False
        simulateDFA "1" dfa `shouldBe` True
        simulateDFA "01" dfa `shouldBe` True
        simulateDFA "11" dfa `shouldBe` False
        simulateDFA "0000001" dfa `shouldBe` True

      it "Should successfully simulate (0*|1*) on a minimized DFA" $ do
        let Right regex = parseRegex "(0*|1*)"
            dfa :: DFA (Set Int) Char = minimize $ subset alphabet (thompsons regex)
        simulateDFA "" dfa `shouldBe` True
        simulateDFA "01" dfa `shouldBe` False
        simulateDFA "0" dfa `shouldBe` True
        simulateDFA "1" dfa `shouldBe` True
        simulateDFA (replicate 100 '0') dfa `shouldBe` True
        simulateDFA (replicate 100 '1') dfa `shouldBe` True

      it "Should successfully simulate (0|1) on a MBP" $ do
        let Right regex = parseRegex "(0|1)"
            dfa :: DFA (Set Int) Char = minimize $ subset alphabet (thompsons regex)
            test x = simulateMBP x (toMatrices (length x) dfa)
        test "0" `shouldBe` True
        test "1" `shouldBe` True
        test "c" `shouldBe` False

      it "Should successfully simulate (0*1) on a MBP" $ do
        let Right regex = parseRegex "(0*1)"
            dfa :: DFA (Set Int) Char = minimize $ subset alphabet (thompsons regex)
            test x = simulateMBP x (toMatrices (length x) dfa)
        test "1" `shouldBe` True
        test "01" `shouldBe` True
        test "11" `shouldBe` False
        test "0000001" `shouldBe` True

      it "Should successfully simulate (0*|1*) on a MBP" $ do
        let Right regex = parseRegex "(0*|1*)"
            dfa :: DFA (Set Int) Char = minimize $ subset alphabet (thompsons regex)
            test x = simulateMBP x (toMatrices (length x) dfa)
        test "01" `shouldBe` False
        test "0" `shouldBe` True
        test "1" `shouldBe` True
        test (replicate 100 '0') `shouldBe` True
        test (replicate 100 '1') `shouldBe` True

      it "Should successfully simulate (0*|1*)010(0*|0*) on a MBP" $ do
        let Right regex = parseRegex "(0*|1*)010(0*|1*)"
            dfa :: DFA (Set Int) Char = minimize $ subset alphabet (thompsons regex)
            test x = simulateMBP x (toMatrices (length x) dfa)
        test "0" `shouldBe` False
        test "1" `shouldBe` False
        test "000" `shouldBe` False
        test "010" `shouldBe` True
        test "000010111" `shouldBe` True

      it "Should successfully simulate 01110101000010010110011010000011 on a MBP" $ do
        let Right regex = parseRegex "01110101000010010110011010000011"
            dfa :: DFA (Set Int) Char = minimize $ subset alphabet (thompsons regex)
            test x = simulateMBP x (toMatrices (length x) dfa)
        test "01110101000010010110011010000011" `shouldBe` True
        test "01110101000010010110011010000010" `shouldBe` False
        test "b" `shouldBe` False
        test "c" `shouldBe` False
        test "abc" `shouldBe` False
        test "aabcaa" `shouldBe` False
        test "01" `shouldBe` False
        test "11" `shouldBe` False

      it "Should successfully simulate 1(0|1)(0|1)1(0|1)01(0|1)000001101010101010(0|1)101(0|1)(0|1) on a MBP" $ do
        let Right regex = parseRegex "1(0|1)(0|1)1(0|1)01(0|1)000001101010101010(0|1)101(0|1)(0|1)"
            dfa :: DFA (Set Int) Char = minimize $ subset alphabet (thompsons regex)
            test x = simulateMBP x (toMatrices (length x) dfa)
        test "10010010000001101010101010110101" `shouldBe` True
        test "11111011000001101010101010110111" `shouldBe` True
        test "b" `shouldBe` False
        test "c" `shouldBe` False
        test "abc" `shouldBe` False
        test "aabcaa" `shouldBe` False
        test "01" `shouldBe` False
        test "11" `shouldBe` False

      it "Should successfully simulate (0|1)*0101010000110000(0|1)* on a MBP" $ do
        let Right regex = parseRegex "(0|1)*0101010000110000(0|1)*"
            dfa :: DFA (Set Int) Char = minimize $ subset alphabet (thompsons regex)
            test x = simulateMBP x (toMatrices (length x) dfa)
        test "0101010000110000" `shouldBe` True
        test "010101010100001100000101" `shouldBe` True
        test "b" `shouldBe` False
        test "c" `shouldBe` False
        test "abc" `shouldBe` False
        test "aabcaa" `shouldBe` False
        test "01" `shouldBe` False
        test "11" `shouldBe` False

      it "Should successfully simulate 01110101000010010110011010000011 on a premultiplied MBP" $ do
        let Right regex = parseRegex "01110101000010010110011010000011"
            input = "01110101000010010110011010000011" :: String
            dfa :: DFA (Set Int) Char = minimize $ subset alphabet (thompsons regex)
            test n = simulateMBPChunks n input $ toMatrices (length input) dfa
        test 16 `shouldBe` True
        test 8  `shouldBe` True
        test 4 `shouldBe` True
        test 2 `shouldBe` True
        test 1 `shouldBe` True

      it "Should successfully simulate 1(0|1)(0|1)1(0|1)01(0|1)000001101010101010(0|1)101(0|1)(0|1) on a premultiplied MBP" $ do
        let Right regex = parseRegex "1(0|1)(0|1)1(0|1)01(0|1)000001101010101010(0|1)101(0|1)(0|1)"
            input = "10010010000001101010101010010100" :: String
            dfa :: DFA (Set Int) Char = minimize $ subset alphabet (thompsons regex)
            test n = simulateMBPChunks n input $ toMatrices (length input) dfa
        test 16 `shouldBe` True
        test 8  `shouldBe` True
        test 4 `shouldBe` True
        test 2 `shouldBe` True
        test 1 `shouldBe` True

      it "Should successfully simulate (0|1)*0101010000110000(0|1)* on a MBP" $ do
        let Right regex = parseRegex "(0|1)*0101010000110000(0|1)*"
            input = "000001010100001100001111" :: String
            dfa :: DFA (Set Int) Char = minimize $ subset alphabet (thompsons regex)
            test n = simulateMBPChunks n input $ toMatrices (length input) dfa
        test 12 `shouldBe` True
        test 8  `shouldBe` True
        test 4 `shouldBe` True
        test 3 `shouldBe` True
        test 2 `shouldBe` True
        test 1 `shouldBe` True
