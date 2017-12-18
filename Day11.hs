module Main where

import Text.Parser.Char
import Text.Parser.Combinators
import Text.Trifecta.Parser
import Text.Trifecta.Result

-- X: s|se
-- Y: n|ne
-- Z: se|ne

data Direction = N | NE | SE | S | SW | NW
data HexVector = HV { _x :: Int
                    , _y :: Int
                    , _z :: Int
                    }

dirVector :: Direction -> HexVector
dirVector N  = HV (-1)  1   0
dirVector NE = HV   0   1 (-1)
dirVector SE = HV   1   0 (-1)
dirVector S  = HV   1 (-1)  0
dirVector SW = HV   0 (-1)  1
dirVector NW = HV (-1)  0   1

addHex :: HexVector -> HexVector -> HexVector
addHex (HV x1 y1 z1) (HV x2 y2 z2) = HV (x1 + x2) (y1 + y2) (z1 + z2)

normHex :: HexVector -> Int
normHex (HV x y z) = maximum [ abs x, abs y, abs z ]

directionP :: Parser Direction
directionP = choice [ NE <$ string "ne"
                    , NW <$ string "nw"
                    , N  <$ string "n"
                    , SE <$ string "se"
                    , SW <$ string "sw"
                    , S  <$ string "s"
                    ]

pathP :: Parser [Direction]
pathP = directionP `sepBy1` char ','

main :: IO ()
main = do input <- getContents
          case parseString (pathP <* eof) mempty input of
            Failure e -> print $ _errDoc e
            -- Success dirs -> let childPos = foldl addHex (HV 0 0 0) $ map dirVector dirs in
            --                 print $ normHex childPos
            Success dirs -> let trace = scanl addHex (HV 0 0 0) $ map dirVector dirs in
                                print $ maximum $ map normHex trace
