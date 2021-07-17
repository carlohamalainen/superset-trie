module Generators where

import qualified Data.List      as List

import Test.QuickCheck

name :: Gen String
name = listOf1 $ choose ('a', 'z')

nameAndValue :: Gen ([String], Int)
nameAndValue = (,) <$> listOf1 name <*> arbitrary

namesAndValues :: Gen [([String], Int)]
namesAndValues = listOf1 nameAndValue

nonEmptySubListOf :: [a] -> Gen [a]
nonEmptySubListOf x = do
    xs <- sublistOf x

    case xs of
        [] -> pure <$> elements x
        _  -> return xs


sortNub :: (Eq a, Ord a) => [a] -> [a]
sortNub = List.nub . List.sort
