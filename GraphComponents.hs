module GraphComponents where

import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Graph a = M.Map a [a]

reachable :: Ord a => Graph a -> a -> S.Set a
reachable g u = search [u] (S.singleton u)
  where search []     found = found
        search (v:vs) found = search (new ++ vs) (foldl (flip S.insert) found new)
          where neighbors = M.findWithDefault [] v g
                new = filter (\w -> not (w `S.member` found)) neighbors

findComponents :: Ord a => Graph a -> [S.Set a]
findComponents g = divide $ S.fromList $ M.keys g
  where divide orphans | S.null orphans = []
        divide orphans = newComp : divide (S.difference orphans newComp)
          where newComp = reachable g $ S.findMin orphans
