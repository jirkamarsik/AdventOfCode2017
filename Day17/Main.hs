{-# LANGUAGE BangPatterns #-}

module Main where

import ListZipperB

import Data.List (foldl')

type SpinBuffer = ListZipper Int

initSpinBuffer :: SpinBuffer
initSpinBuffer = LZ [0] []

stepBuffer :: Int -> SpinBuffer -> Int -> SpinBuffer
stepBuffer skipSize sb newElem = advanceC 1 $ insert newElem $ advanceC skipSize sb

data SpinState = SS { _after0 :: Int
                    , _index  :: Int
                    , _size   :: Int
                    } deriving Show

initSpinState :: SpinState
initSpinState = SS 0 0 1

stepSS :: Int -> SpinState -> Int -> SpinState
stepSS skipSize (SS !after0 !index !size) newElem = SS newAfter0 newIndex newSize
  where index' = (index + skipSize) `mod` size
        newAfter0 = if index' == 0 then newElem else after0
        newIndex = index' + 1
        newSize = size + 1

main :: IO ()
main = do skipSize <- read <$> getContents
          -- let finalBuffer = foldl (stepBuffer skipSize) initSpinBuffer [1..2017]
          -- print $ headC finalBuffer
          let finalState = foldl' (stepSS skipSize) initSpinState [1..50000000]
          print $ _after0 finalState
