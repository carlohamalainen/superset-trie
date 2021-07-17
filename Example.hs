module Main where

import SupersetTrie

import Control.Monad
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
    let names :: [ ([String], Int) ]
        names =
            [ (["foo", "bar", "baz"],   0)
            , (["apple", "cat"],        1)
            , (["dog"],                 2)
            ]

        t = toTrie names

    pPrint t

    let sets =
          [ ["foo", "bar"]
          , ["dog"]
          , ["crab"]
          , ["bar", "baz"]
          , ["cat", "apple"]
          , ["foo", "bar", "baz", "blerp"]
          ]

    forM_ sets $ \s -> do
        print (s, s `member` t, s `find` t)
