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
buildTree = foldl insertPair Map.empty
  where
    -- Inserting pairs into the tree dictionary
    insertPair tree (parent, child) = Map.insertWith (++) parent [child] tree

-- Function to find the deepest children given a parent key
deepestChildren :: Map.Map String [String] -> String -> Maybe [String]
deepestChildren tree key = sort <$> deepestHelper tree [key]

-- Helper function for deepestChildren
deepestHelper :: Map.Map String [String] -> [String] -> Maybe [String]
deepestHelper _ [] = Nothing -- Base case: empty list, no more parents to explore
deepestHelper tree parents =
    let children = concatMap (\p -> fromMaybe [] (Map.lookup p tree)) parents
    in if null children
       then Just parents -- If no children found at this level, return the parents as deepest children
       else deepestHelper tree children

-- Function to convert Maybe [String] to String
convertToString :: Maybe [String] -> String
convertToString = maybe "" unwords

main :: IO ()
main = do
    -- Get command-line arguments
    args <- getArgs
    -- Check if a filename is provided
    if null args
        then putStrLn "Usage: ./gerarquia test.txt"
    else do
        let filename = head args                        -- Extract the filename from command-line arguments
        contents <- readFile filename                   -- Read contents from the file
        let pairsList = lines contents                  -- Split file contents into lines
            rootNode = head pairsList                   -- Extract the root node from the first line
            pairs = createPairs (tail pairsList)        -- Create pairs from the remaining lines
            tree = buildTree pairs                      -- Build a tree from the pairs
            deepest = deepestChildren tree rootNode     -- Find the deepest children for the root node
        putStrLn (convertToString deepest)              -- Print the deepest children