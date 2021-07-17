{-# LANGUAGE ScopedTypeVariables    #-}

module SupersetTrie where

import qualified Data.Map.Lazy  as M
import qualified Data.List      as List

import Control.Monad
import Data.Maybe

data Trie a b = Trie Bool (Maybe b) (M.Map a (Trie a b))
  deriving (Eq, Read, Show)

emptyTrie :: Trie a b
emptyTrie = Trie False Nothing M.empty

insert :: Ord a => [a] -> b -> Trie a b -> Trie a b
insert [] = error "Can't insert empty values"
insert x = insert' (List.nub $ List.sort x)
  where
    insert' :: Ord a => [a] -> b -> Trie a b -> Trie a b
    insert' [] b (Trie _ _ nodes) = Trie True (Just b) nodes
    insert' (y:ys) b (Trie end _b nodes)
        = Trie end (Just b) $
            M.alter (Just . insert' ys b . fromMaybe emptyTrie')
                y
                nodes

    emptyTrie' = Trie True Nothing M.empty

toTrie :: (Foldable t, Ord a) => t ([a], b) -> Trie a b
toTrie = foldl (\t (a, b) -> insert a b t) emptyTrie

keys :: Ord a => [a] -> Trie a b -> [[a]]
keys acc (Trie False _ nodes)
    | M.null nodes
    = error "what"

    | otherwise
    = List.sort $ concatMap (\(k, n) -> keys (k:acc) n) $ M.toList nodes

keys acc (Trie True _ nodes)
    | M.null nodes
    = [acc]

    | otherwise
    = List.sort $ concatMap (\(k, n) -> keys (k:acc) n) $ M.toList nodes

member :: (Ord a, Show a, Show b) => [a] -> Trie a b -> Bool
member = member' . List.nub . List.sort
  where
    member' [] (Trie end _ _) = end

    member' (x:xs) (Trie _ _ nodes) = with || without
      where

        -- (1) Use x, which means finding a child trie labeled x.
        with = Just True == (member' xs <$> M.lookup x nodes)

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

        without = any (member' (x:xs) . snd) $ M.toList nodes'

find :: Ord a => [a] -> Trie a b -> Maybe b
find = find' . List.nub . List.sort 
  where
    find' :: Ord a => [a] -> Trie a b -> Maybe b

    find' [] (Trie end b _) = if end then b else Nothing

    find' (x:xs) (Trie _ _ nodes) = foldl mplus with without
      where
        with = find' xs =<< M.lookup x nodes

        -- Similar optimisation as in member'.
        nodes' = M.filterWithKey (\k _ -> k < x) nodes
        without = map (find' (x:xs) . snd) $ M.toList nodes'
