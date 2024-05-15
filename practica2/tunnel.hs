import System.IO
import System.Environment

main :: IO ()
main = do
    -- Get command-line arguments
    args <- getArgs
    -- Check if a filename is provided
    if null args
        then putStrLn "Usage: ./tunnel test.txt"
    else do
        let filename = head args                        -- Extract the filename from command-line arguments
        contents <- readFile filename                   -- Read contents from the file
        let params = lines contents                     -- Split file contents into lines
            primeraLinia = head params                  -- Extract the root node from the first line
            capacitat = last (words primeraLinia)       -- Get the maximum number of umpalumpas in the tunnel
            llista = last params                        -- Obtenir llista de Umpalumpas 
        putStrLn (capacitat)              -- Print the deepest children
        putStrLn llista