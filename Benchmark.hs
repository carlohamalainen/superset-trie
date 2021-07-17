{-# LANGUAGE ScopedTypeVariables    #-}

module Main where

import SupersetTrie
import Generators

import qualified Data.Set       as Set

import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Random

import qualified Criterion.Main         as C

main :: IO ()
main = do
    let xs = unGen namesAndValues (mkQCGen 40) 8000

    print $ length xs

    let t = toTrie xs
        s = map (Set.fromList . fst) xs

        lists = map fst xs

        listIn xs ys = all (`elem` ys) xs

    let searchSubsets = map (\seed -> take 50 $ unGen (elements xs >>= nonEmptySubListOf . fst) (mkQCGen seed) 15) [1..10]

    print $ length xs
    print $ map length searchSubsets

    let setFind s x = head $ filter (\ss -> Set.fromList x `Set.isSubsetOf` ss) s

        listFind lists x = head $ filter (listIn x) lists

    C.defaultMain [
        C.bgroup "subset-find"
            [ C.bench "subset-trie" $ C.nf (map (`find` t))         searchSubsets
            , C.bench "Data.Set"    $ C.nf (map (setFind s))        searchSubsets
            , C.bench "naive-lists" $ C.nf (map (listFind lists))   searchSubsets
            ]
        ]
