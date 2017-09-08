{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Data.Data
import Data.Set
import Test.Hspec

import Regex

main :: IO ()
main =
  hspec $ do
    describe "regex-fsm tests" $ do
      it "Should successfully simulate (a|b) on an ENFA" $ do
        let Right regex = parseRegex "(a|b)"
            thompson = thompsons regex :: ENFA Int Char
        simulateENFA "a" thompson `shouldBe` True
        simulateENFA "b" thompson  `shouldBe` True
        simulateENFA "c" thompson `shouldBe` False
        simulateENFA "" thompson `shouldBe` False

      it "Should successfully simulate (a*b) on an ENFA" $ do
        let Right regex = parseRegex "(a*b)"
            thompson = thompsons regex :: ENFA Int Char
        simulateENFA "" thompson `shouldBe` False
        simulateENFA "b" thompson `shouldBe` True
        simulateENFA "ab" thompson `shouldBe` True
        simulateENFA "bb" thompson `shouldBe` False
        simulateENFA "aaaaab" thompson `shouldBe` True

      it "Should successfully simulate (a*|b*) on an ENFA" $ do
        let Right regex = parseRegex "(a*|b*)"
            thompson = thompsons regex :: ENFA Int Char
        simulateENFA "" thompson `shouldBe` True
        simulateENFA "ab" thompson `shouldBe` False
        simulateENFA "a" thompson `shouldBe` True
        simulateENFA "b" thompson `shouldBe` True
        simulateENFA (replicate 100 'a') thompson `shouldBe` True
        simulateENFA (replicate 100 'b') thompson `shouldBe` True

      it "Should successfully simulate (a|b) on a DFA" $ do
        let Right regex = parseRegex "(a|b)"
            dfa :: DFA (Set Int) Char = subset (thompsons regex)
        simulateDFA "a" dfa `shouldBe` True
        simulateDFA "b" dfa `shouldBe` True
        simulateDFA "c" dfa `shouldBe` False
        simulateDFA "" dfa `shouldBe` False

      it "Should successfully simulate (a*b) on a DFA" $ do
        let Right regex = parseRegex "(a*b)"
            dfa :: DFA (Set Int) Char = subset (thompsons regex)
        simulateDFA "" dfa `shouldBe` False
        simulateDFA "b" dfa `shouldBe` True
        simulateDFA "ab" dfa `shouldBe` True
        simulateDFA "bb" dfa `shouldBe` False
        simulateDFA "aaaaab" dfa `shouldBe` True

      it "Should successfully simulate (a*|b*) on a DFA" $ do
        let Right regex = parseRegex "(a*|b*)"
            dfa :: DFA (Set Int) Char = subset (thompsons regex)
        simulateDFA "" dfa `shouldBe` True
        simulateDFA "ab" dfa `shouldBe` False
        simulateDFA "a" dfa `shouldBe` True
        simulateDFA "b" dfa `shouldBe` True
        simulateDFA (replicate 100 'a') dfa `shouldBe` True
        simulateDFA (replicate 100 'b') dfa `shouldBe` True

      it "Should successfully simulate (a|b) on a minimized DFA" $ do
        let Right regex = parseRegex "(a|b)"
            dfa :: DFA (Set Int) Char = minimize $ subset (thompsons regex)
        simulateDFA "a" dfa `shouldBe` True
        simulateDFA "b" dfa `shouldBe` True
        simulateDFA "c" dfa `shouldBe` False
        simulateDFA "" dfa `shouldBe` False

      it "Should successfully simulate (a*b) on a minimized DFA" $ do
        let Right regex = parseRegex "(a*b)"
            dfa :: DFA (Set Int) Char = minimize $ subset (thompsons regex)
        simulateDFA "" dfa `shouldBe` False
        simulateDFA "b" dfa `shouldBe` True
        simulateDFA "ab" dfa `shouldBe` True
        simulateDFA "bb" dfa `shouldBe` False
        simulateDFA "aaaaab" dfa `shouldBe` True

      it "Should successfully simulate (a*|b*) on a minimized DFA" $ do
        let Right regex = parseRegex "(a*|b*)"
            dfa :: DFA (Set Int) Char = minimize $ subset (thompsons regex)
        simulateDFA "" dfa `shouldBe` True
        simulateDFA "ab" dfa `shouldBe` False
        simulateDFA "a" dfa `shouldBe` True
        simulateDFA "b" dfa `shouldBe` True
        simulateDFA (replicate 100 'a') dfa `shouldBe` True
        simulateDFA (replicate 100 'b') dfa `shouldBe` True

      it "Should successfully simulate (a|b) on a MBP" $ do
        let Right regex = parseRegex "(a|b)"
            dfa :: DFA (Set Int) Char = minimize $ subset (thompsons regex)
            test x = simulateMBP x (toMatrices (length x) dfa)
        test "a" `shouldBe` True
        test "b" `shouldBe` True
        test "c" `shouldBe` False

      it "Should successfully simulate (a*b) on a MBP" $ do
        let Right regex = parseRegex "(a*b)"
            dfa :: DFA (Set Int) Char = minimize $ subset (thompsons regex)
            test x = simulateMBP x (toMatrices (length x) dfa)
        test "b" `shouldBe` True
        test "ab" `shouldBe` True
        test "bb" `shouldBe` False
        test "aaaaab" `shouldBe` True

      it "Should successfully simulate (a*|b*) on a MBP" $ do
        let Right regex = parseRegex "(a*|b*)"
            dfa :: DFA (Set Int) Char = minimize $ subset (thompsons regex)
            test x = simulateMBP x (toMatrices (length x) dfa)
        test "ab" `shouldBe` False
        test "a" `shouldBe` True
        test "b" `shouldBe` True
        test (replicate 100 'a') `shouldBe` True
        test (replicate 100 'b') `shouldBe` True

      it "Should successfully simulate (a*|b*)abc(a*|b*) on a MBP" $ do
        let Right regex = parseRegex "(a*|b*)abc(a*|b*)"
            dfa :: DFA (Set Int) Char = minimize $ subset (thompsons regex)
            test x = simulateMBP x (toMatrices (length x) dfa)
        test "a" `shouldBe` False
        test "b" `shouldBe` False
        test "c" `shouldBe` False
        test "abc" `shouldBe` True
        test "aabcaa" `shouldBe` True

      it "Should successfully simulate 01110101000010010110011010000011 on a MBP" $ do
        let Right regex = parseRegex "01110101000010010110011010000011"
            dfa :: DFA (Set Int) Char = minimize $ subset (thompsons regex)
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
            dfa :: DFA (Set Int) Char = minimize $ subset (thompsons regex)
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
            dfa :: DFA (Set Int) Char = minimize $ subset (thompsons regex)
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
            dfa :: DFA (Set Int) Char = minimize $ subset (thompsons regex)
            test n = simulateMBPChunks n input $ toMatrices (length input) dfa
        test 16 `shouldBe` True
        test 8  `shouldBe` True
        test 4 `shouldBe` True
        test 2 `shouldBe` True
        test 1 `shouldBe` True

      it "Should successfully simulate 1(0|1)(0|1)1(0|1)01(0|1)000001101010101010(0|1)101(0|1)(0|1) on a premultiplied MBP" $ do
        let Right regex = parseRegex "1(0|1)(0|1)1(0|1)01(0|1)000001101010101010(0|1)101(0|1)(0|1)"
            input = "10010010000001101010101010010100" :: String
            dfa :: DFA (Set Int) Char = minimize $ subset (thompsons regex)
            test n = simulateMBPChunks n input $ toMatrices (length input) dfa
        test 16 `shouldBe` True
        test 8  `shouldBe` True
        test 4 `shouldBe` True
        test 2 `shouldBe` True
        test 1 `shouldBe` True

      it "Should successfully simulate (0|1)*0101010000110000(0|1)* on a MBP" $ do
        let Right regex = parseRegex "(0|1)*0101010000110000(0|1)*"
            input = "000001010100001100001111" :: String
            dfa :: DFA (Set Int) Char = minimize $ subset (thompsons regex)
            test n = simulateMBPChunks n input $ toMatrices (length input) dfa
        test 12 `shouldBe` True
        test 8  `shouldBe` True
        test 4 `shouldBe` True
        test 3 `shouldBe` True
        test 2 `shouldBe` True
        test 1 `shouldBe` True
