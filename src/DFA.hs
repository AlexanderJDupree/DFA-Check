-- |
-- Module      :  DFA
-- Description :  Defines DFA type and implements associated functions
-- Copyright   :  Copyright Alexander DuPree (c) 2019
-- Maintainer  :  Alexander DuPree
-- Stability   :  experimental
-- Portability :  POSIX

{-# LANGUAGE ExistentialQuantification #-}

module DFA
    ( DFA (..)
    , eval
    ) where

import Data.Set ( Set )
import qualified Data.Set as Set

data DFA state symbol = Ord state => DFA { states   :: Set state
                                         , alphabet :: Set symbol
                                         , delta    :: state -> symbol -> state
                                         , start    :: state
                                         , accept   :: Set state
                                         }

eval :: Ord state => DFA state symbol -> [symbol] -> Bool
eval dfa word = isAccept dfa $ foldl (delta dfa) (start dfa) word 

isAccept :: Ord state => DFA state symbol -> state -> Bool
isAccept dfa st = Set.member st $ accept dfa

