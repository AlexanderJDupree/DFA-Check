-- |
-- Module      :  OneXYSpec
-- Description :  Unit tests for DFA-Check library
-- Copyright   :  Copyright Alexander DuPree (c) 2019
-- Maintainer  :  Alexander DuPree
-- Stability   :  experimental
-- Portability :  POSIX

{-# LANGUAGE OverloadedLists #-}

module OneXYSpec where

import           DFA
import           Test.Hspec

-- list States
data State = Q0 | Q1 | Q2 | Q3 | Q4 | Q5 | Q6 | Q7
    deriving (Eq, Ord, Read, Show)

type Symbol = Char

-- Transition function
m1Delta :: State -> Symbol -> State
m1Delta Q0 '0' = Q0
m1Delta Q0 '1' = Q1

m1Delta Q1 '0' = Q2
m1Delta Q1 '1' = Q5

m1Delta Q2 '0' = Q3
m1Delta Q2 '1' = Q4

m1Delta Q3 '0' = Q0
m1Delta Q3 '1' = Q1

m1Delta Q4 '0' = Q2
m1Delta Q4 '1' = Q5

m1Delta Q5 '0' = Q6
m1Delta Q5 '1' = Q7

m1Delta Q6 '0' = Q3
m1Delta Q6 '1' = Q4

m1Delta Q7 '0' = Q6
m1Delta Q7 '1' = Q7

m1 = DFA { states   = [Q0, Q1, Q2, Q3, Q4, Q5, Q6, Q7]
         , alphabet = ['0', '1']
         , delta    = m1Delta
         , start    = Q0
         , accept   = [Q3, Q4, Q6, Q7]
         }

-- Accepted strings
language  = [ "111"
            , "110"
            , "101"
            , "100"
            , "10101"
            , "10100"
            , "100100"
            , "1000100"
            , "1111100"
            ]

-- Rejected strings
language' = [ "0"
            , "10"
            , "1010"
            , "01011"
            , "111000"
            , "1001001"
            ]

allTrue :: Int -> [Bool]
allTrue n = replicate n True

allFalse :: Int -> [Bool]
allFalse n = replicate n False

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "3rd symbol RHS is a 1"
    $ context "when given any valid string"
    $ it "evaluates to true"
    $ map (eval m1) language `shouldBe` allTrue (length language)

  describe "3rd symbol RHS is a 1"
    $ context "when given any invalid string"
    $ it "evaluates to false"
    $ map (eval m1) language' `shouldBe` allFalse (length language')

