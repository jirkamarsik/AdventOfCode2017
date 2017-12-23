{-# LANGUAGE BangPatterns #-}

module Main where

import ListZipperB

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Trifecta.Parser
import Text.Trifecta.Result

type Reg = Char
data Val = RegV Reg | NumV Integer
data Inst = Snd Val | Upd (Integer -> Integer -> Integer) Reg Val | Rcv Reg | Jgz Val Val
type Prog = [Inst]
type PC = ListZipper Inst
type Env = Map Reg Integer

type MemState = (PC, Env)
data ThreadState' = Done | Waiting (Integer -> ThreadState)
type ThreadState = ([Integer], ThreadState')

regP :: Parser Reg
regP = letter

numP :: Parser Integer
numP = do sign <- option 1 (-1 <$ char '-')
          value <- read <$> some digit
          return $ sign * value

valP :: Parser Val
valP = choice [ RegV <$> regP
              , NumV <$> numP
              ]

updOpP :: Parser (Integer -> Integer -> Integer)
updOpP = choice [ (\_ y -> y) <$ string "set"
                , (+) <$ string "add"
                , try $ (*) <$ string "mul"
                , mod <$ string "mod"
                ]

instP :: Parser Inst
instP = choice [ try $ Snd <$ string "snd" <* spaces <*> valP
               , Upd <$> updOpP <* spaces <*> regP <* spaces <*> valP
               , Rcv <$ string "rcv" <* spaces <*> regP
               , Jgz <$ string "jgz" <* spaces <*> valP <* spaces <*> valP
               ]

programP :: Parser Prog
programP = instP `sepEndBy1` newline

-- run :: Prog -> Maybe Integer
-- run prog = exec (LZ [] prog) M.empty 0
--   where exec :: PC -> Env -> Integer -> Maybe Integer
--         exec (LZ ls (i@(Snd v):rs))      e s = exec (LZ (i:ls) rs) e (eval e v)
--         exec (LZ ls (i@(Upd op r v):rs)) e s = exec (LZ (i:ls) rs) (M.insert r (op (eval e (RegV r)) (eval e v)) e) s
--         exec (LZ ls (i@(Rcv r):rs))      e s = if eval e (RegV r) /= 0 then
--                                                  Just s
--                                                else
--                                                  exec (LZ (i:ls) rs) e s
--         exec pc@(LZ ls (i@(Jgz c o):rs)) e s = if eval e c > 0 then
--                                                  case move (eval e o) pc of
--                                                    Just pc' -> exec pc' e s
--                                                    Nothing -> Nothing
--                                                else
--                                                  exec (LZ (i:ls) rs) e s
--         exec (LZ ls [])                  _ _ = Nothing
--         eval :: Env -> Val -> Integer
--         eval e (RegV r) = M.findWithDefault 0 r e
--         eval _ (NumV n) = n

run :: Prog -> Int
run prog = simulate initA initB 0
  where initA :: ThreadState
        initA = exec (LZ [] prog) (M.singleton 'p' 0)
        initB :: ThreadState
        initB = exec (LZ [] prog) (M.singleton 'p' 1)
        simulate :: ThreadState -> ThreadState -> Int -> Int
        simulate (_, Done)        (qB, Done)       !acc = acc + length qB
        simulate ([], Waiting _)  ([], Waiting _)  !acc = acc
        simulate (a:as, tsA)      (qB, Waiting kB) !acc = let (qB', tsB) = kB a in
                                                            simulate (as, tsA) (qB ++ qB', tsB) acc
        simulate (qA, Waiting kA) (b:bs, tsB)      !acc  = let (qA', tsA) = kA b in
                                                             simulate (qA ++ qA', tsA) (bs, tsB) (acc + 1)
        exec :: PC -> Env -> ThreadState
        exec (LZ ls (i@(Snd v):rs))      e = let (sends, state) = exec (LZ (i:ls) rs) e in
                                             (eval e v : sends, state)
        exec (LZ ls (i@(Upd op r v):rs)) e = exec (LZ (i:ls) rs) (M.insert r (op (eval e (RegV r)) (eval e v)) e)
        exec (LZ ls (i@(Rcv r):rs))      e = ([], Waiting (\v -> exec (LZ (i:ls) rs) (M.insert r v e)))
        exec pc@(LZ ls (i@(Jgz c o):rs)) e = if eval e c > 0 then
                                               case move (eval e o) pc of
                                                 Just pc' -> exec pc' e
                                                 Nothing -> ([], Done)
                                             else
                                               exec (LZ (i:ls) rs) e
        exec (LZ ls [])                  _ = ([], Done)
        eval :: Env -> Val -> Integer
        eval e (RegV r) = M.findWithDefault 0 r e
        eval _ (NumV n) = n

main :: IO ()
main = do input <- getContents
          case parseString (programP <* eof) mempty input of
            Failure e -> print $ _errDoc e
            Success prog -> print $ run prog
