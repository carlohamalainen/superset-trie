{-# LANGUAGE ScopedTypeVariables    #-}

module SupersetTrie where

import qualified Data.Map.Lazy  as M
import qualified Data.List      as List

import Control.Monad
import Data.Maybe
import Data.Foldable

import Generators (sortNub)

data STrie a b = STrie (Maybe b) (M.Map a (STrie a b))
  deriving (Eq, Read, Show)

emptySTrie :: STrie a b
emptySTrie = STrie Nothing M.empty

insert :: Ord a => [a] -> b -> STrie a b -> STrie a b
insert [] = error "Can't insert empty values"
insert x = insert' $ sortNub x
  where
    insert' :: Ord a => [a] -> b -> STrie a b -> STrie a b
    insert' [] b (STrie _ nodes) = STrie (Just b) nodes
    insert' (y:ys) b (STrie _b nodes)
        = STrie (Just b) $
            M.alter (Just . insert' ys b . fromMaybe emptySTrie')
                y
                nodes

    emptySTrie' = STrie Nothing M.empty

toSTrie :: (Foldable t, Ord a) => t ([a], b) -> STrie a b
toSTrie = foldl (\t (a, b) -> insert a b t) emptySTrie

keys :: Ord a => [a] -> STrie a b -> [[a]]
keys acc (STrie _ nodes)
    | M.null nodes
    = [acc]

    | otherwise
    = List.sort $ concatMap (\(k, n) -> keys (k:acc) n) $ M.toList nodes

member :: (Ord a) => [a] -> STrie a b -> Bool
member = (isJust .) . SupersetTrie.find

find :: Ord a => [a] -> STrie a b -> Maybe b
find [] _ = Nothing
find k strie = find' (sortNub k) strie
  where
    find' :: Ord a => [a] -> STrie a b -> Maybe b

    find' [] (STrie b _) = b

    find' (x:xs) (STrie _ nodes) = foldl' mplus with without
      where

        -- (1) Use x, which means finding a child trie labeled x.
        with = find' xs =<< M.lookup x nodes

        -- (2) Skip x, which means finding some child tree where
        -- the recursive call works. Since x:xs is sorted and the
        -- values inserted into the trie are sorted, we don't
        -- need to look at values where x is >= the label.
        --
        -- For example if x = "foo" and the children are
        --
        -- "bar", "egg", "zed"
        -- 
        -- then there's no point following the "zed" sub-trie
        -- since we will never find a "foo" there, due
        -- to the fact that "zed" > "foo".
        nodes' = M.filterWithKey (\k _ -> k < x) nodes

        without = map (find' (x:xs) . snd) $ M.toList nodes'
