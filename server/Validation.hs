{-# LANGUAGE QuasiQuotes, FlexibleContexts #-}
module Validation where

import qualified Data.Text as T 
import           Data.Maybe
import qualified Data.MonoTraversable as MT
import           Text.Regex.PCRE.Heavy

type Validation e a = a -> Maybe e

validate :: (a -> b) -> [Validation e a] -> a -> Either [e] b
validate constructor validations val =
  case concatMap (\f -> maybeToList $ f val) validations of
    []   -> Right $ constructor val
    errs -> Left errs

rangeBetween :: (Ord a) => a -> a -> e -> Validation e a
rangeBetween minRange maxRange msg val =
  if val >= minRange && val <= maxRange then Nothing else Just msg

lengthBetween :: (MT.MonoTraversable a) => Int -> Int -> e -> Validation e a
lengthBetween minLen maxLen msg val =
  rangeBetween minLen maxLen msg (MT.olength val)

regexMatches :: Regex -> e -> Validation e T.Text
regexMatches regex msg val =
  if val =~ regex then Nothing else Just msg
