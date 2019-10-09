-- |
-- Module      :  M1Spec
-- Description :  Unit tests for DFA-Check library
-- Copyright   :  Copyright Alexander DuPree (c) 2019
-- Maintainer  :  Alexander DuPree
-- Stability   :  experimental
-- Portability :  POSIX

{-# LANGUAGE OverloadedLists #-}

module M1Spec where

import DFA
import Test.Hspec

-- list States
data State = Q0 |Q1
    deriving (Eq, Ord, Read, Show)

type Symbol = Char

-- Transition function
m1Delta :: State -> Symbol -> State
m1Delta Q0 '0' = Q0
m1Delta Q0 '1'  = Q1
m1Delta Q1 '0' = Q0
m1Delta Q1 '1'  = Q1

m1 = DFA { states   = [Q0, Q1]
         , alphabet = ['0', '1']
         , delta    = m1Delta
         , start    = Q0
         , accept   = [Q1]
         }

-- Accepted strings
language  = ["1", "01", "1011", "1001"]
-- Rejected strings
language' = ["0", "10", "1010"]

allTrue :: Int -> [Bool]
allTrue n = replicate n True

allFalse :: Int -> [Bool]
allFalse n = replicate n False

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Odd Binary String recognizer"
    $ context "when given any odd binary string"
    $ it "evaluates to true"
    $ map (eval m1) language `shouldBe` allTrue 4 

  describe "Odd Binary String recognizer"
    $ context "when given any even binary string"
    $ it "evaluates to false"
    $ map (eval m1) language' `shouldBe` allFalse 3

