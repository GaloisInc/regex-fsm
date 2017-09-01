{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map             as M
import           Data.Set
import           System.Process
import           Test.Hspec

import           Regex

main :: IO ()
main =
  hspec $ do
    describe "obfuscator tests" $ do
      it "Should successfully zero-test using the obfuscator tool, without security" $ do
        testWithEvalObfuscator "0" "0" 1 True
        testWithEvalObfuscator "(01|10)" "10" 2 True
        testWithEvalObfuscator "101010101" "101010101" (length ("101010101" :: String)) True
        testWithEvalObfuscator "(0|1)*" "010101" 6 True
        testWithEvalObfuscator "0*" "000" 3 True
        testWithEvalObfuscator "(0|1)*" (concat $ replicate 200 "01") 400 True
        testWithEvalObfuscator "(0|1)*0101010000110000(0|1)*" "0101010000110000" (length ("0101010000110000" :: String)) True
        testWithEvalObfuscator "1(0|1)(0|1)1(0|1)01(0|1)000001101010101010(0|1)101(0|1)(0|1)" "10010010000001101010101010110101" (length ("10010010000001101010101010110101" :: String)) True
        testWithEvalObfuscator "01110101000010010110011010000011" "01110101000010010110011010000011" (length ("01110101000010010110011010000011" :: String)) True
        testWithEvalObfuscator "01110101000010010110011010000011" "01110101000010010110011010000010" (length ("01110101000010010110011010000010" :: String)) False

      it "Should successfully zero-test using the obfuscator tool, with security" $ do
        testObfuscatorWithSecurity "0" "0" 1 50 True
        testObfuscatorWithSecurity "(01|10)" "01" 2 50 True
        testObfuscatorWithSecurity "(01|10)" "10" 2 50 True
        testObfuscatorWithSecurity "101010101" "101010101" (length ("101010101" :: String)) 40 True

        --  flint-addons.c:40: fmpz_mat_det_modp: Assertion `n >= 1' failed.
        -- testObfuscatorWithSecurity "0*" "000" 3 40 True -- strange case

        --  flint-addons.c:40: fmpz_mat_det_modp: Assertion `n >= 1' failed.
        -- testObfuscatorWithSecurity "(0|1)*" (concat $ replicate 200 "01") 400 40 True

        testObfuscatorWithSecurity "(0|1)*0101010000110000(0|1)*" "0101010000110000" (length ("0101010000110000" :: String)) 40 True
        testObfuscatorWithSecurity "1(0|1)(0|1)1(0|1)01(0|1)000001101010101010(0|1)101(0|1)(0|1)" "10010010000001101010101010110101" (length ("10010010000001101010101010110101" :: String)) 40 True
        testObfuscatorWithSecurity "01110101000010010110011010000011" "01110101000010010110011010000011" (length ("01110101000010010110011010000011" :: String)) 40 True
        testObfuscatorWithSecurity "01110101000010010110011010000011" "01110101000010010110011010000010" (length ("01110101000010010110011010000010" :: String)) 40 False

type RegexString = String
type ProcessArg = String

testWithEvalObfuscator :: RegexString -> ProcessArg -> Int -> Bool -> IO ()
testWithEvalObfuscator str arg n r = do
  let Right regex = parseRegex str
      dfa :: DFA (Set Int) Char = minimize $ subset (thompsons regex)
      test = Matrices n $ M.mapKeys pure <$> toMatrices n dfa
      fileName = "output.json"
  BL.writeFile fileName (encode test)
  (code, out, err) <- readProcessWithExitCode "obfuscator"
    [ "bp"
    , "--load"
    , fileName
    , "--eval"
    , arg
    ] []
  void $ system "rm -r output.json"
  case words out of
    ["Output", "=", x] ->
       case read x :: Int of
         n' -> (if n' == 0 then True else False) `shouldBe` r
    _ -> do
      print code
      error err

type SecParam = Int
type Length = Int

testObfuscatorWithSecurity
  :: RegexString
  -> ProcessArg
  -> Length
  -> SecParam
  -> Bool
  -> IO ()
testObfuscatorWithSecurity str arg n secParam r = do
  let Right regex = parseRegex str
      dfa :: DFA (Set Int) Char = minimize $ subset (thompsons regex)
      test = Matrices n $ M.mapKeys pure <$> toMatrices n dfa
      fileName = "output.json"
  BL.writeFile fileName (encode test)
  result <- readProcessWithExitCode "obfuscator"
    [ "obf"
    , "--load"
    , fileName
    , "--secparam"
    , show secParam
    ] []
  print result
  (code, out, err) <- readProcessWithExitCode "obfuscator"
    [ "obf"
    , "--load-obf"
    , fileName ++ ".obf." ++ show secParam
    , "--eval"
    , arg
    ] []
  void $ system "rm -r output.json.obf*"
  case words out of
    ["Output", "=", x] ->
       case read x :: Int of
         n' -> (if n' == 0 then True else False) `shouldBe` r
    _ -> do
      print code
      error err
