{-# LANGUAGE ScopedTypeVariables    #-}

module Main where

import SupersetTrie
import Generators

import qualified Data.Set       as Set

import Data.Bifunctor

import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Random

import qualified Criterion.Main         as C

main :: IO ()
main = do
    let xs :: [([String], Int)]
        xs = unGen namesAndValues (mkQCGen 40) 8000

    print $ length xs

    let t = toSTrie xs

        s :: [(Set.Set String, Int)]
        s = map (first Set.fromList) xs

        listIn xs ys = all (`elem` ys) xs

    let searchSubsets = map (\seed -> take 50 $ unGen (elements xs >>= nonEmptySubListOf . fst) (mkQCGen seed) 15) [1..10]

    print $ length xs
    print $ map length searchSubsets

    let setFind sets x = head $ filter (\ss -> Set.fromList x `Set.isSubsetOf` fst ss) sets

        listFind lists x = head $ filter (listIn x . fst) lists

    C.defaultMain [
        C.bgroup "subset-find"
            [ C.bench "subset-trie" $ C.nf (map (`find` t))    searchSubsets
            , C.bench "Data.Set"    $ C.nf (map (setFind s))   searchSubsets
            , C.bench "naive-lists" $ C.nf (map (listFind xs)) searchSubsets
            ]
        ]
