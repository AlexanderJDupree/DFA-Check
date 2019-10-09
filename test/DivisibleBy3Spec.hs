-- |
-- Module      :  DivisibleBy3Spec
-- Description :  Unit tests for DFA-Check library
-- Copyright   :  Copyright Alexander DuPree (c) 2019
-- Maintainer  :  Alexander DuPree
-- Stability   :  experimental
-- Portability :  POSIX

{-# LANGUAGE OverloadedLists #-}

module DivisibleBy3Spec where

import DFA
import Test.Hspec

-- list States
data State = Q0 |Q1 | Q2
    deriving (Eq, Ord, Read, Show)

type Symbol = Char

-- Transition function
m1Delta :: State -> Symbol -> State
m1Delta Q0 '0' = Q0
m1Delta Q0 '1'  = Q1

m1Delta Q1 '0' = Q2
m1Delta Q1 '1'  = Q0

m1Delta Q2 '0' = Q1
m1Delta Q2 '1'  = Q2

m1 = DFA { states   = [Q0, Q1, Q2]
         , alphabet = ['0', '1']
         , delta    = m1Delta
         , start    = Q0
         , accept   = [Q0]
         }

-- Accepted strings
language  = ["", "0", "11", "110", "1001", "1100"]
-- Rejected strings
language' = ["01", "10", "1010", "1110", "1011"]

allTrue :: Int -> [Bool]
allTrue n = replicate n True

allFalse :: Int -> [Bool]
allFalse n = replicate n False

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Binary strings divisible by 3"
    $ context "when given any binary string divisble by 3"
    $ it "evaluates to true"
    $ map (eval m1) language `shouldBe` allTrue 6

  describe "Binary strings divisible by 3"
    $ context "when given any binary string not divisble by 3"
    $ it "evaluates to false"
    $ map (eval m1) language' `shouldBe` allFalse 5

