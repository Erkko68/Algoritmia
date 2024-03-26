import System.IO
import System.Environment
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.List (sort)

-- Function to create a list of pairs from a list of strings
createPairs :: [String] -> [(String, String)]
createPairs = map createPair

-- Function to create a pair from a string in the format "A B"
createPair :: String -> (String, String)
createPair str = case words str of
    (father:child:_) -> (father, child)
    _ -> error "Invalid format. Expected 'Father child'."

-- Function to build the tree dictionary from the list of parent-child pairs
buildTree :: [(String, String)] -> Map.Map String [String]
buildTree pairs = foldl insertPair Map.empty pairs
  where
    insertPair tree (parent, child) = Map.insertWith (++) parent [child] tree

-- Function to find the deepest children given a parent key
deepestChildren :: Map.Map String [String] -> String -> [String]
deepestChildren tree key = sort $ deepestHelper tree [key]

deepestHelper :: Map.Map String [String] -> [String] -> [String]
deepestHelper _ [] = [] -- Base case: empty list, no more parents to explore
deepestHelper tree parents =
    let children = concatMap (\p -> fromMaybe [] (Map.lookup p tree)) parents
    in if null children
       then parents -- If no children found at this level, return the parents as deepest children
       else deepestHelper tree children

main :: IO ()
main = do
    -- Get user args
    args <- getArgs
    if null args
        then putStrLn "Usage: ./llinatge filename.txt"
    else do
        let filename = head args -- Assign filename to args variable
        -- Save file contents
        contents <- readFile filename
        -- Convert each file line into pairs
        let parelles = lines contents
            node = head parelles
            pairs = createPairs (tail parelles)
            tree = buildTree pairs
            deepest = deepestChildren tree node
        putStrLn (unwords deepest)
        