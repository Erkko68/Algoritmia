
import System.Environment

tunnel :: Int -> [Int] -> Int
-- Falta implementar

stringToTuple :: [String] -> (Int, Int)
stringToTuple [x, y] = (read x, read y)

stringsToInts :: [String] -> [Int]
stringsToInts = map read

-- main = putStrLn $ show $ tunel 2 [1,2,10,5]
main :: IO()
main = do
   args <- getArgs
   contents <- readFile (head args)

   let [first_line, second_line] = lines contents
   let (capacity, number) = stringToTuple $ words first_line
   let umpalumpas = stringsToInts $ words second_line

   print (backtracking capacity umpalumpas [] True 0)


