{-# LANGUAGE BangPatterns #-}

module Main where

import ListZipperA

import Data.Maybe (fromJust)
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Trifecta.Parser
import Text.Trifecta.Result

type Alphabet = Bool
type Tape = ListZipper Alphabet
newtype State = State String deriving Eq

data Direction = ToTheLeft | ToTheRight
data Action = Action { _newValue  :: Alphabet
                     , _direction :: Direction
                     , _newState  :: State }
data Automaton = Auto { _initState   :: State
                      , _transitions :: State -> Alphabet -> Action
                      , _steps       :: Int }

stateP :: Parser State
stateP = State <$> some letter

alphabetP :: Parser Alphabet
alphabetP = choice [ True <$ char '1'
                   , False <$ char '0' ]

directionP :: Parser Direction
directionP = choice [ ToTheLeft <$ string "left"
                    , ToTheRight <$ string "right" ]

nonNegDecimalP :: Parser Int
nonNegDecimalP = read <$> some digit

stateSpecP :: Parser (State, Alphabet -> Action)
stateSpecP = do state <- string "In state " *> stateP <* string ":" <* spaces
                actionBlocks <- some (try actionBlockP)
                return (state, \a -> fromJust $ lookup a actionBlocks)
  where actionBlockP :: Parser (Alphabet, Action)
        actionBlockP = do oldValue <- string "If the current value is " *> alphabetP <* string ":" <* spaces
                          newValue <- string "- Write the value " *> alphabetP <* string "." <* spaces
                          direction <- string "- Move one slot to the " *> directionP <* string "." <* spaces
                          newState <- string "- Continue with state " *> stateP <* string "." <* spaces
                          return (oldValue, Action newValue direction newState)

automatonP :: Parser Automaton
automatonP = do initState <- string "Begin in state " *> stateP <* string "." <* spaces
                steps <- string "Perform a diagnostic checksum after " *> nonNegDecimalP <* string " steps." <* spaces
                stateSpecs <- some stateSpecP
                return (Auto initState (\s -> fromJust $ lookup s stateSpecs) steps)

advance :: (State -> Alphabet -> Action) -> (State, Tape) -> (State, Tape)
advance transit (!state, tape@(LZ _ !_ _)) = (newState, newTape)
  where (Action !newValue !direction !newState) = transit state (getCursor tape)
        newTape = fromJust $ move (case direction of ToTheLeft -> -1; ToTheRight -> 1) $ updateCursor (const newValue) tape

main :: IO ()
main = do input <- getContents
          case parseString (automatonP <* eof) mempty input of
            Failure e -> print $ _errDoc e
            Success (Auto initState transitions steps) ->
              let initTape = LZ (repeat False) False (repeat False)
                  (_, finalTape) = iterate (advance transitions) (initState, initTape) !! steps
                  (LZ ls h rs) = finalTape
                  reachableCells = h : take steps ls ++ take steps rs in
                print $ length $ filter id reachableCells
