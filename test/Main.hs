{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Boltzmann.Data
import Control.Monad
import Data.Data
import Debug.Trace
import Data.Matrix

import Test.Hspec
import Test.Hspec.QuickCheck
import qualified Data.Map as M
import Test.QuickCheck
import Text.Show.Pretty

import Regex

instance (Data a, Arbitrary a) => Arbitrary (Reg a) where
  arbitrary = sized $ generatorPWith [positiveInts]

positiveInts :: Alias Gen
positiveInts = alias $ \() ->
  getPositive <$> arbitrary :: Gen Int

main :: IO ()
main =
  hspec $ do
    describe "regex-fsm tests" $ do
      it "Should successfully simulate (a|b) on an ENFA" $ do
        let Right regex = parseRegex "(a|b)"
        simulateENFA "a" (thompsons regex) `shouldBe` True
        simulateENFA "b" (thompsons regex) `shouldBe` True
        simulateENFA "c" (thompsons regex) `shouldBe` False
        simulateENFA "" (thompsons regex) `shouldBe` False

      it "Should successfully simulate (a*b) on an ENFA" $ do
        let Right regex = parseRegex "(a*b)"
        simulateENFA "" (thompsons regex) `shouldBe` False
        simulateENFA "b" (thompsons regex) `shouldBe` True
        simulateENFA "ab" (thompsons regex) `shouldBe` True
        simulateENFA "bb" (thompsons regex) `shouldBe` False
        simulateENFA "aaaaab" (thompsons regex) `shouldBe` True

      it "Should successfully simulate (a*|b*) on an ENFA" $ do
        let Right regex = parseRegex "(a*|b*)"
        simulateENFA "" (thompsons regex) `shouldBe` True
        simulateENFA "ab" (thompsons regex) `shouldBe` False
        simulateENFA "a" (thompsons regex) `shouldBe` True
        simulateENFA "b" (thompsons regex) `shouldBe` True
        simulateENFA (replicate 100 'a') (thompsons regex) `shouldBe` True
        simulateENFA (replicate 100 'b') (thompsons regex) `shouldBe` True

      it "Should successfully simulate (a|b) on a DFA" $ do
        let Right regex = parseRegex "(a|b)"
            dfa = subset (thompsons regex)
        simulateDFA "a" dfa `shouldBe` True
        simulateDFA "b" dfa `shouldBe` True
        simulateDFA "c" dfa `shouldBe` False
        simulateDFA "" dfa `shouldBe` False

      it "Should successfully simulate (a*b) on a DFA" $ do
        let Right regex = parseRegex "(a*b)"
            dfa = subset (thompsons regex)
        simulateDFA "" dfa `shouldBe` False
        simulateDFA "b" dfa `shouldBe` True
        simulateDFA "ab" dfa `shouldBe` True
        simulateDFA "bb" dfa `shouldBe` False
        simulateDFA "aaaaab" dfa `shouldBe` True

      it "Should successfully simulate (a*|b*) on a DFA" $ do
        let Right regex = parseRegex "(a*|b*)"
            dfa = subset (thompsons regex)
        simulateDFA "" dfa `shouldBe` True
        simulateDFA "ab" dfa `shouldBe` False
        simulateDFA "a" dfa `shouldBe` True
        simulateDFA "b" dfa `shouldBe` True
        simulateDFA (replicate 100 'a') dfa `shouldBe` True
        simulateDFA (replicate 100 'b') dfa `shouldBe` True

      it "Should successfully simulate (a|b) on a minimized DFA" $ do
        let Right regex = parseRegex "(a|b)"
            dfa = minimize $ subset (thompsons regex)
        simulateDFA "a" dfa `shouldBe` True
        simulateDFA "b" dfa `shouldBe` True
        simulateDFA "c" dfa `shouldBe` False
        simulateDFA "" dfa `shouldBe` False

      it "Should successfully simulate (a*b) on a minimized DFA" $ do
        let Right regex = parseRegex "(a*b)"
            dfa = minimize $ subset (thompsons regex)
        simulateDFA "" dfa `shouldBe` False
        simulateDFA "b" dfa `shouldBe` True
        simulateDFA "ab" dfa `shouldBe` True
        simulateDFA "bb" dfa `shouldBe` False
        simulateDFA "aaaaab" dfa `shouldBe` True

      it "Should successfully simulate (a*|b*) on a minimized DFA" $ do
        let Right regex = parseRegex "(a*|b*)"
            dfa = minimize $ subset (thompsons regex)
        simulateDFA "" dfa `shouldBe` True
        simulateDFA "ab" dfa `shouldBe` False
        simulateDFA "a" dfa `shouldBe` True
        simulateDFA "b" dfa `shouldBe` True
        simulateDFA (replicate 100 'a') dfa `shouldBe` True
        simulateDFA (replicate 100 'b') dfa `shouldBe` True

  --     it "Should successfully simulate (a*|b*) on a MBP" $ do
  --       let Right regex = parseRegex "(a*|b*)"
  --           dfa = minimize $ subset (thompsons regex)
  --       let (result, final) = toMatrices ("a" :: String) dfa
  --       -- forM_ (M.assocs final) $ \(a,f) ->
  --       --   do putStrLn (pure a)
  --       --      putStrLn (prettyMatrix f)
  --       putStrLn "result"
  --       pPrint result
  --       putStrLn "final"
  --       pPrint final
  --       pPrint $ simulateMBP ("a" :: String) result final

        -- print $ simulateMBP "bbbbb" (toMatrices ("bbbbb" :: String) dfa) -- ( 1 0 0 )

        -- print $ simulateMBP "a" (toMatrices ("a" :: String) dfa) -- ( 1 0 )
        -- print $ simulateMBP "b" (toMatrices ("b" :: String) dfa) -- ( 0 1 )

--        print $ simulateMBP "" (toMatrices ("" :: String) dfa)

        -- simulateDFA "" dfa `shouldBe` True
        -- simulateDFA "ab" dfa `shouldBe` False
        -- simulateDFA "a" dfa `shouldBe` True
        -- simulateDFA "b" dfa `shouldBe` True
        -- simulateDFA (replicate 100 'a') dfa `shouldBe` True
        -- simulateDFA (replicate 100 'b') dfa `shouldBe` True

      -- it "Should successfully simulate (a|b) on a matrix-branching program" $ do
      --   let Right regex = parseRegex "(a|b)"
      --       mbp = toMatrices $ minimize $ subset (thompsons regex)
      --   simulateMBP "a" mbp `shouldBe` True
      --   simulateMBP "b" mbp `shouldBe` True
      --   simulateMBP "c" mbp `shouldBe` False
      --   simulateMBP "" mbp `shouldBe` False

      -- it "Should successfully simulate (a*b) on a matrix-branching program" $ do
      --   let Right regex = parseRegex "(a*b)"
      --       mbp = toMatrices $ minimize $ subset (thompsons regex)
      --   simulateMBP "" mbp `shouldBe` False
      --   simulateMBP "b" mbp `shouldBe` True
      --   simulateMBP "ab" mbp `shouldBe` True
      --   simulateMBP "bb" mbp `shouldBe` False
      --   simulateMBP "aaaaab" mbp `shouldBe` True

      -- it "Should successfully simulate (a*|b*) on a matrix-branching program" $ do
      --   let Right regex = parseRegex "(a*|b*)"
      --       mbp = toMatrices $ minimize $ subset (thompsons regex)
      --   pPrint $ simulateMBP "ab" mbp
      --   simulateMBP "" mbp `shouldBe` True
      --   simulateMBP "ab" mbp `shouldBe` False
      --   simulateMBP "a" mbp `shouldBe` True
      --   simulateMBP "b" mbp `shouldBe` True
      --   simulateMBP (replicate 100 'a') mbp `shouldBe` True
      --   simulateMBP (replicate 100 'b') mbp `shouldBe` True


      -- it "Should produce the same simulated results on all regex" $ do
      --   property $ \((input:: String, regex :: Reg Char)) ->
      --     traceShow (input, regex) $ do
      --       let enfa = thompsons regex
      --           mbp = subset enfa
      --           mmbp = minimize mbp
      --           a = simulateENFA input enfa
      --           b = simulateMBP input mbp
      --           c = simulateMBP input mmbp
      --       traceShow (ppShow mbp) $ a == b `shouldBe` b == c

