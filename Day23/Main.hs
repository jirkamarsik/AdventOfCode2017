module Main where

import Duet
import ListZipperB

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Trifecta.Parser
import Text.Trifecta.Result

eval :: Env -> Val -> Integer
eval e (RegV r) = Map.findWithDefault 0 r e
eval _ (NumV n) = n

debugMuls :: Prog -> Integer
debugMuls prog = runCountMuls (LZ [] prog) Map.empty 0
  where runCountMuls :: PC -> Env -> Integer -> Integer
        runCountMuls (LZ ls (i@(Upd op r v):rs))      e c = runCountMuls (LZ (i:ls) rs)
                                                                         (Map.insert r (runUpdOp op (eval e (RegV r))
                                                                                                    (eval e v)) e)
                                                                         (c + if op == Mul then 1 else 0)
        runCountMuls (pc@(LZ ls (i@(Jump t v o):rs))) e c = if test t (eval e v) then
                                                              case move (eval e o) pc of
                                                                Just pc' -> runCountMuls pc' e c
                                                                Nothing -> c
                                                            else
                                                              runCountMuls (LZ (i:ls) rs) e c
        runCountMuls (LZ _ [])                        _ c = c

run :: Prog -> Env -> Env
run prog env = exec (LZ [] prog) env
  where exec :: PC -> Env -> Env
        exec (LZ ls (i@(Upd op r v):rs))    e = exec (LZ (i:ls) rs) (Map.insert r (runUpdOp op (eval e (RegV r)) (eval e v)) e)
        exec pc@(LZ ls (i@(Jump t c o):rs)) e = if test t (eval e c) then
                                                  case move (eval e o) pc of
                                                    Just pc' -> exec pc' e
                                                    Nothing -> e
                                                else
                                                  exec (LZ (i:ls) rs) e
        exec (LZ _ [])                      e = e

optimisedProgramResult :: Int
optimisedProgramResult = length $ filter (not . isPrime) series
  where series = takeWhile (<= 81 * 100 + 100000 + 17000) $ iterate (+ 17) (81 * 100 + 100000)
        isPrime :: Int -> Bool
        isPrime n = null $ filter (\d -> n `mod` d == 0) $ takeWhile (\i -> i * i <= n) [2..n]

main :: IO ()
main = do -- input <- getContents
          -- case parseString (programP <* eof) mempty input of
          --   Failure e -> print $ _errDoc e
          --   Success prog -> print $ debugMuls prog
          print $ optimisedProgramResult
