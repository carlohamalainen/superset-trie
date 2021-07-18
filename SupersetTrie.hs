{-# LANGUAGE ScopedTypeVariables    #-}

module SupersetTrie where

import qualified Data.Map.Lazy  as M
import qualified Data.List      as List

import Control.Monad
import Data.Maybe

import Generators (sortNub)

data STrie a b = STrie Bool (Maybe b) (M.Map a (STrie a b))
  deriving (Eq, Read, Show)

emptySTrie :: STrie a b
emptySTrie = STrie False Nothing M.empty

insert :: Ord a => [a] -> b -> STrie a b -> STrie a b
insert [] = error "Can't insert empty values"
insert x = insert' $ sortNub x
  where
    insert' :: Ord a => [a] -> b -> STrie a b -> STrie a b
    insert' [] b (STrie _ _ nodes) = STrie True (Just b) nodes
    insert' (y:ys) b (STrie end _b nodes)
        = STrie end (Just b) $
            M.alter (Just . insert' ys b . fromMaybe emptySTrie')
                y
                nodes

    emptySTrie' = STrie True Nothing M.empty

toSTrie :: (Foldable t, Ord a) => t ([a], b) -> STrie a b
toSTrie = foldl (\t (a, b) -> insert a b t) emptySTrie

keys :: Ord a => [a] -> STrie a b -> [[a]]
keys acc (STrie False _ nodes)
    | M.null nodes
    = error "what"

    | otherwise
    = List.sort $ concatMap (\(k, n) -> keys (k:acc) n) $ M.toList nodes

keys acc (STrie True _ nodes)
    | M.null nodes
    = [acc]

    | otherwise
    = List.sort $ concatMap (\(k, n) -> keys (k:acc) n) $ M.toList nodes

member :: (Ord a) => [a] -> STrie a b -> Bool
member = member' . sortNub
  where
    member' [] (STrie end _ _) = end

    member' (x:xs) (STrie _ _ nodes) = with || without
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

find :: Ord a => [a] -> STrie a b -> Maybe b
find = find' . sortNub 
  where
    find' :: Ord a => [a] -> STrie a b -> Maybe b

    find' [] (STrie end b _) = if end then b else Nothing

    find' (x:xs) (STrie _ _ nodes) = foldl mplus with without
      where
        with = find' xs =<< M.lookup x nodes

        -- Similar optimisation as in member'.
        nodes' = M.filterWithKey (\k _ -> k < x) nodes
        without = map (find' (x:xs) . snd) $ M.toList nodes'
