{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Monad.State.Strict
import Data.Attoparsec.Text
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (pack)

type Env = M.Map String Int
type Expr a = State Env a
type Proc = Expr ()

identifierP :: Parser String
identifierP = many1 letter

numberP :: Parser Int
numberP = signed decimal

relOpP :: Parser (Int -> Int -> Bool)
relOpP = choice [ (<=) <$ string "<="
                , (<)  <$ string "<"
                , (==) <$ string "=="
                , (/=) <$ string "!="
                , (>=) <$ string ">="
                , (>)  <$ string ">"
                ]

conditionP :: Parser (Expr Bool)
conditionP = do register <- identifierP
                char ' '
                relOp <- relOpP
                char ' '
                right <- numberP
                return $ do left <- gets (M.findWithDefault 0
                                          register)
                            return $ left `relOp` right

statementP :: Parser Proc
statementP = do register <- identifierP
                char ' '
                sign <- 1 <$ string "inc" <|> -1 <$ string "dec"
                char ' '
                diff <- numberP
                char ' '
                string "if"
                char ' '
                condition <- conditionP
                return $ do cond <- condition
                            if cond then
                              modify (M.alter (\x -> Just $ fromMaybe 0 x + sign * diff) register)
                            else
                              return ()

-- programP :: Parser Proc
programP :: Parser [Proc]
programP = do actions <- statementP `sepBy1` endOfLine
              endOfInput
              -- return $ sequnce_ actions
              return actions

dump :: Expr Env
dump = get

main :: IO ()
main = do input <- pack <$> getContents
          case parseOnly programP input of
            Left e -> putStrLn e
            -- Right prog -> let endState = execState prog M.empty
            --                   topValue = maximum $ M.elems endState in
            Right prog -> let allStates = evalState (sequence (map (>> dump) prog)) M.empty
                              topValue = maximum $ concatMap M.elems allStates in
                              print topValue
