{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}

module Main where

import SupersetTrie
import Generators

import qualified Data.List      as List
import qualified Data.Set       as Set

import Test.QuickCheck

import qualified Test.QuickCheck.Property as P

-- We can find subsets.
prop_memberSubsets :: Gen P.Result
prop_memberSubsets = do
    xs <- namesAndValues

    let t = toTrie xs

    x' <- elements xs >>= nonEmptySubListOf . fst

    return $ if x' `member` t
                then P.succeeded
                else P.failed { P.reason = show x' }

-- Testing membership matches the result using Data.Set.
prop_membership :: Gen P.Result
prop_membership = do
    xs <- namesAndValues

    let xs' = map fst xs
    
        names = List.nub $ List.sort $ concatMap fst xs

        t = toTrie xs

        s = map Set.fromList xs'

    sub <- nonEmptySubListOf names

    let isSubsetMember = sub `member` t
        isSubsetSets   = any (Set.fromList sub `Set.isSubsetOf`) s

    return $ if isSubsetMember == isSubsetSets
        then P.succeeded
        else P.failed { P.reason = show sub }

-- Find and match agree.
prop_findVsMatch :: Gen P.Result
prop_findVsMatch = do
    xs <- namesAndValues

    let t = toTrie xs

    x' <- elements xs >>= nonEmptySubListOf . fst

    let m = x' `member` t
        f = x' `find`   t

    return $ case (m, f) of
        (True,  Just _)  -> P.succeeded
        (False, Nothing) -> P.succeeded
        _                -> P.failed { P.reason = show (m, f, x', t) }

prop_keysRoundTrip :: Gen P.Result
prop_keysRoundTrip = do
    xs <- namesAndValues

    let xs' = sortNub $ map (sortNub . fst) xs
    
        t = toTrie xs

        k = sortNub $ map List.sort $ keys [] t

    return $ if xs' == k
        then P.succeeded
        else P.failed { P.reason = show (xs', k) }

return [] -- https://hackage.haskell.org/package/QuickCheck-2.14.2/docs/Test-QuickCheck-All.html
          -- Note: the bizarre return [] in the example above is needed on GHC 7.8 and later
main :: IO Bool
main = $quickCheckAll
