import System.Environment

remEle :: Eq a => [a] -> a -> [a]
remEle llista element = filter (/= element) llista

combinacions llista mida solucio
    | length llista < mida = [llista]
    | length solucio == mida = [solucio]
    | otherwise = foldr (\element acc -> acc ++ combinacions llista mida (solucio ++ [element])) [] filteredList
        where filteredList = [x | x <- llista, x `notElem` solucio]

remList :: Eq a => [a] -> [a] -> [a]
remList llista elements = foldl (\acc element -> remEle acc element) llista elements

tunel :: Int -> [Int] -> [Int] -> Bool -> Int -> [Int]
tunel capacitat fora dins entrem temps
    | fora == [] = [temps]
    | entrem = foldl (++) [] (map (\esquadra ->
      let novaFora = remList fora esquadra
          nousDins = dins ++ esquadra
          nouTemps = temps + maximum esquadra
      in tunel capacitat novaFora nousDins (not entrem) nouTemps) (combinacions fora capacitat []))
    | otherwise = foldl (++) [] (map (\umpalumpa ->
      let novaFora = fora ++ [umpalumpa]
          nousDins = remEle dins umpalumpa
          nouTemps = temps + umpalumpa
      in tunel capacitat novaFora nousDins (not entrem) nouTemps) (dins))


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
    let (_, capacity) = stringToTuple $ words first_line
    let umpalumpas = stringsToInts $ words second_line

    let result = tunel capacity umpalumpas [] True 0
    --putStrLn $ "Result: " ++ show result
    if null result
        then putStrLn "0"
        else putStrLn $ show (minimum result)
    --mapM_ print result
