# superset-trie

Trie data structure supporting superset-key search.

Based on: Iztok Savnik. Index Data Structure for Fast Subset and Superset Queries. 1st Cross-Domain Con-
ference and Workshop on Availability, Reliability, and Security in Information Systems (CD-ARES),
Sep 2013, Regensburg, Germany. pp.134-148. [pdf](978-3-642-40511-2_10_Chapter.pdf)

## Build

    cabal build --enable-tests

## Tests

Run ``cabal test`` or

    $ dist-newstyle/build/x86_64-linux/ghc-8.8.4/superset-trie-0.1.0.0/t/test-superset-trie/build/test-superset-trie/test-superset-trie
    === prop_memberSubsets from Tests.hs:17 ===
    +++ OK, passed 100 tests.

    === prop_membership from Tests.hs:30 ===
    +++ OK, passed 100 tests.

    === prop_findVsMatch from Tests.hs:52 ===
    +++ OK, passed 100 tests.

    === prop_keysRoundTrip from Tests.hs:68 ===
    +++ OK, passed 100 tests.

## Example

In the shell, the output will be nicely coloured using [pretty-simple](https://hackage.haskell.org/package/pretty-simple).

    $ dist-newstyle/build/x86_64-linux/ghc-8.8.4/superset-trie-0.1.0.0/x/example/build/example/example
    Trie False
        ( Just 2 )
        ( fromList
            [
                ( "apple"
                , Trie True
                    ( Just 1 )
                    ( fromList
                        [
                            ( "cat"
                            , Trie True
                                ( Just 1 )
                                ( fromList [] )
                            )
                        ]
                    )
                )
            ,
                ( "bar"
                , Trie True
                    ( Just 0 )
                    ( fromList
                        [
                            ( "baz"
                            , Trie True
                                ( Just 0 )
                                ( fromList
                                    [
                                        ( "foo"
                                        , Trie True
                                            ( Just 0 )
                                            ( fromList [] )
                                        )
                                    ]
                                )
                            )
                        ]
                    )
                )
            ,
                ( "dog"
                , Trie True
                    ( Just 2 )
                    ( fromList [] )
                )
            ]
        )
    (["foo","bar"],True,Just 0)
    (["dog"],True,Just 2)
    (["crab"],False,Nothing)
    (["bar","baz"],True,Just 0)
    (["cat","apple"],True,Just 1)
    (["foo","bar","baz","blerp"],False,Nothing)

## Benchmark

Criterion output: [superset-trie.html](superset-trie.html)

![subset-trie criterion benchmarks](subset-trie-benchmarks.png)

    $ dist-newstyle/build/x86_64-linux/ghc-8.8.4/superset-trie-0.1.0.0/x/benchmark/build/benchmark/benchmark --output=superset-trie.html
    2884
    2884
    [50,50,50,50,50,50,50,50,50,50]
    benchmarking subset-find/subset-trie
    time                 658.6 ms   (613.6 ms .. 718.8 ms)
                         0.999 R²   (0.998 R² .. 1.000 R²)
    mean                 645.3 ms   (634.2 ms .. 655.2 ms)
    std dev              11.84 ms   (8.426 ms .. 14.44 ms)
    variance introduced by outliers: 19% (moderately inflated)

    benchmarking subset-find/Data.Set
    time                 2.626 s    (2.463 s .. 2.950 s)
                         0.998 R²   (0.996 R² .. 1.000 R²)
    mean                 2.570 s    (2.536 s .. 2.609 s)
    std dev              45.96 ms   (18.30 ms .. 61.96 ms)
    variance introduced by outliers: 19% (moderately inflated)

    benchmarking subset-find/naive-lists
    time                 11.66 s    (8.715 s .. 16.03 s)
                         0.980 R²   (0.973 R² .. 1.000 R²)
    mean                 9.750 s    (9.155 s .. 10.75 s)
    std dev              962.3 ms   (146.5 ms .. 1.240 s)
    variance introduced by outliers: 22% (moderately inflated)
