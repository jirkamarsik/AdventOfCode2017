{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}

module Main where

import Control.Lens
import Control.Monad (replicateM_, when)
import Control.Monad.State.Strict
import Data.Maybe (fromMaybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data NodeState = Clean | Weakened | Infected | Flagged deriving Eq
data Node = N Int Int deriving (Eq, Ord)
data Dir = D Int Int

data VirusState = VS { _stateMap :: (Map Node NodeState)
                     , _curNode :: Node
                     , _curDir :: Dir
                     , _numInfections :: !Integer }
makeLenses ''VirusState

(<+>) :: Node -> Dir -> Node
N x y <+> D dx dy = N (x + dx) (y + dy)

turnLeft :: Dir -> Dir
turnLeft (D dx dy) = D dy (-dx)

turnRight :: Dir -> Dir
turnRight (D dx dy) = D (-dy) dx

turnAround :: Dir -> Dir
turnAround = turnLeft . turnLeft

curState :: MonadState VirusState m => m NodeState
curState = Map.findWithDefault Clean <$> use curNode <*> use stateMap

nextState :: NodeState -> NodeState
-- nextState Clean    = Infected
-- nextState Infected = Clean
nextState Clean    = Weakened
nextState Weakened = Infected
nextState Infected = Flagged
nextState Flagged  = Clean

virusBurst :: (MonadState VirusState m) => m ()
virusBurst = do turn
                infect
                advance
  where turn = do state <- curState
                  curDir %= case state of
                              Clean -> turnLeft
                              Weakened -> id
                              Infected -> turnRight
                              Flagged -> turnAround
        infect = do node <- use curNode
                    stateMap %= Map.alter (\prev -> Just $ nextState $ fromMaybe Clean prev) node
                    newState <- curState
                    when (newState == Infected) $ numInfections += 1
        advance = do dir <- use curDir
                     curNode %= (<+> dir)

main :: IO ()
main = do grid <- lines <$> getContents
          let infected = Map.fromList $ [ (N x y, state) | (y, row) <- zip [0..] grid
                                                         , (x, c) <- zip [0..] row
                                                         , let state = if c == '#' then Infected else Clean ]
              startPos = N (length (head grid) `div` 2) (length grid `div` 2)
              startDir = D 0 (-1)
              startState = VS infected startPos startDir 0
          -- print $ _numInfections $ execState (replicateM_ 10000 virusBurst) startState
          print $ _numInfections $ execState (replicateM_ 10000000 virusBurst) startState
