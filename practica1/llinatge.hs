import System.IO
import System.Environment
import qualified Data.Map as Map

-- lookup: buscar element, respon amb un maybe si no el troba o l'elemnt

-- Function to create a list of pairs from a list of strings
createPairs :: [String] -> [(String, String)]
createPairs = map createPair


-- Function to create a pair from a string in the format "A B"
createPair :: String -> (String, String)
createPair str = case words str of
    (father:child:_) -> (father, child)
    _ -> error "Invalid format. Expected 'Father child'."

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
            pairs = createPairs parelles
        putStrLn (show parelles)
        putStrLn (show pairs)
            
