module Main where

import Data.Ord (Down(Down))
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Exts (sortWith)
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Trifecta.Parser
import Text.Trifecta.Result

data Component = C { _from :: Int
                   , _to :: Int
                   , _ident :: Int } deriving (Eq, Ord)
type Bridge = [Component]

numberP :: Parser Int
numberP = read <$> some digit

componentP :: Parser (Int -> Component)
componentP = C <$> numberP <* char '/' <*> numberP

componentsP :: Parser [Component]
componentsP = zipWith (\i mkComp -> mkComp i) [1..] <$> componentP `sepEndBy1` newline

swap :: Component -> Component
swap (C from to ident) = C to from ident

remove :: Component -> Set Component -> Set Component
remove component = Set.delete component . Set.delete (swap component)

allConnections :: Int -> Set Component -> [Component]
allConnections end = Set.toList .
                     Set.takeWhileAntitone (\(C from _ _) -> from == end) .
                     Set.dropWhileAntitone (\(C from _ _) -> from < end)

allBridges :: Int -> Set Component -> [Bridge]
allBridges start components = [[]] ++
                              do nextConnection <- allConnections start components
                                 rest <- allBridges (_to nextConnection) (remove nextConnection components)
                                 return $ nextConnection : rest

bridgeStrength :: Bridge -> Int
bridgeStrength = sum . map (\(C from to _)-> from + to)

main :: IO ()
main = do input <- getContents
          case parseString (componentsP <* eof) mempty input of
            Failure e -> print $ _errDoc e
            Success componentList -> let components = Set.fromList $ componentList ++ map swap componentList
                                         bridges = allBridges 0 components in
                                     -- print $ maximum $ map bridgeStrength bridges
                                     print $ bridgeStrength $ head $ sortWith (\x -> Down (length x, bridgeStrength x)) bridges
