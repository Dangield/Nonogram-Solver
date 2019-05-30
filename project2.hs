import System.Console.ANSI
import System.IO
import Data.List.Split
import Data.Char
import Data.List

data Nono = N Int Int [[(Int,Color)]] [[(Int,Color)]] [Color] [[[Color]]] deriving Show

solve :: String -> IO ()
solve fname = do handle <- openFile fname ReadMode
                 contents <- hGetContents handle
                 let n = getNonoFromString contents
                 putStrLn("First look at the game.")
                 printGame n
                 fillSafeSpaces n

--retrieving nonogram from file
getNonoFromString :: String -> Nono
getNonoFromString s = do let args = splitOn ";" s
                         let c = read (args !! 0) :: Int
                         let r = read (args !! 1) :: Int
                         let cx = getCmdsFromStrings (splitOn "|" (args !! 2))
                         let rx = getCmdsFromStrings (splitOn "|" (args !! 3))
                         let col = [White] ++ getColorsInNono (cx ++ rx)
                         N c r (enrichCmds (splitCmds cx)) (enrichCmds (splitCmds rx)) col (initSolution c r col)

getCmdsFromStrings :: [String] -> [(Int,Int,Color)]
getCmdsFromStrings [] = []
getCmdsFromStrings (s:ss) = do let args = splitOn "," s
                               [(read (args !! 0) :: Int,read (args !! 1) :: Int,toEnum (read (args !! 2) :: Int) :: Color)] ++ getCmdsFromStrings ss

splitCmds :: [(Int,Int,Color)] -> [[(Int,Color)]]
splitCmds [] = []
splitCmds ((x1,x2,x3):xs) = [[(a2,a3) | (a1,a2,a3) <- ((x1,x2,x3):xs), a1 == x1]] ++ splitCmds [(a1,a2,a3) | (a1,a2,a3) <- ((x1,x2,x3):xs), a1 /= x1] 

enrichCmds :: [[(Int,Color)]] -> [[(Int,Color)]]
enrichCmds [] = []
enrichCmds (x:xs) = [addWhiteCmds x] ++ enrichCmds xs

addWhiteCmds :: [(Int,Color)] -> [(Int,Color)]
addWhiteCmds [] = []
addWhiteCmds (x:[]) = [x]
addWhiteCmds ((x11,x12):(x21,x22):xs) = if x12 == x22 then [(x11,x12),(1,White)] ++ addWhiteCmds ((x21,x22):xs)
                                        else [(x11,x12)] ++ addWhiteCmds ((x21,x22):xs)

getColorsInNono :: [(Int,Int,Color)] -> [Color]
getColorsInNono [] = []
getColorsInNono ((_,_,x):xs) = [x] ++ filter (/=x) (getColorsInNono xs)

initSolution :: Int -> Int -> [Color] -> [[[Color]]]
initSolution c r col = take r (repeat (take c (repeat col)))

--printing game in console
printGame :: Nono -> IO ()
printGame (N c r cx rx _ s) = do let mc = maximum ([length x| x<-cx])
                                 let mr = maximum ([length x| x<-rx])
                                 printColsCmds c cx mc mr
                                 printRowsCmds r rx s mr

printColsCmds :: Int -> [[(Int,Color)]] -> Int -> Int -> IO ()
printColsCmds _ _ 0 _ = putStr("")
printColsCmds _ [] _ _ = putStrLn("")
printColsCmds c (cx:cxs) mc mr = do if length (cx:cxs) == c then putStr(take mr (repeat ' '))
                                    else putStr("")
                                    if length cx >= mc then do let x = cx !! ((length cx)-mc)
                                                               putNumWithColor (fst x) (snd x)
                                                               putStr(" ") 
                                    else putStr("  ")
                                    printColsCmds c cxs mc mr
                                    if length (cx:cxs) == c then printColsCmds c (cx:cxs) (mc-1) mr
                                    else putStr("")

printRowsCmds :: Int -> [[(Int,Color)]] -> [[[Color]]] -> Int -> IO ()
printRowsCmds 0 _ _ _ = putStr("")
printRowsCmds r rx s mr = do printRowsCmds (r-1) rx s mr
                             printRowCmds (rx !! (r-1)) (s !! (r-1)) mr

printRowCmds :: [(Int,Color)] -> [[Color]] -> Int -> IO ()
printRowCmds [] [] _ = putStrLn("")
printRowCmds [] (s:ss) mr = do if length s == 1 then do putCharWithColor ('X') (head s)
                                                        putStr(" ")
                               else putStr("_ ")
                               printRowCmds [] ss mr
printRowCmds rx s mr = if length rx == mr then do putNumWithColor (fst (head rx)) (snd (head rx))
                                                  printRowCmds (tail rx) s (mr-1)
                       else do putStr(" ")
                               printRowCmds rx s (mr-1)

--print char in Color
putNumWithColor :: Int -> Color -> IO ()
putNumWithColor x c = if x<10 then putCharWithColor (intToDigit x) c
                      else putCharWithColor (chr (x+87)) c

putCharWithColor :: Char -> Color -> IO ()
putCharWithColor x c = do setSGR [SetColor Foreground Vivid c]
                          putChar x
                          setSGR [Reset]

--operations on solution
rotateSolution :: [[[Color]]] -> [[[Color]]]
rotateSolution ([]:_) = []
rotateSolution s = ([head x | x<-s]:rotateSolution [tail x | x<-s])

--solve the nonogram - pre main loop - filling safe spaces
fillSafeSpaces :: Nono -> IO ()
fillSafeSpaces (N c r cx rx col s) = do let s1 = fillSafeSpacesInRows (N c r cx rx col s)
                                        putStrLn("Safe spaces in rows filled.")
                                        printGame (N c r cx rx col s1)
                                        let s2 = fillSafeSpacesInCols (N c r cx rx col s1)
                                        putStrLn("Safe spaces in cols filled.")
                                        printGame (N c r cx rx col s2)
                                        tryForSolution (N c r cx rx col s2)

fillSafeSpacesInRows :: Nono -> [[[Color]]]
fillSafeSpacesInRows (N _ _ _ [] _ []) = []
fillSafeSpacesInRows (N c r cx (rx:rxs) col (s:ss)) = do let e = c - sum [x1 | (x1,x2)<-rx]
                                                         if e < maximum [x1 | (x1,x2)<-rx] then [fillSafeSpacesInRow e rx s]++fillSafeSpacesInRows (N c r cx rxs col ss)
                                                         else [s]++fillSafeSpacesInRows (N c r cx rxs col ss)

fillSafeSpacesInRow :: Int -> [(Int,Color)] -> [[Color]] -> [[Color]]
fillSafeSpacesInRow e [] s = s
fillSafeSpacesInRow e ((rx1,rx2):rxs) s = if rx1 > e then (take e s) ++ (take (rx1-e) (repeat [rx2])) ++ (fillSafeSpacesInRow e rxs (drop rx1 s))
                                          else (take rx1 s) ++ (fillSafeSpacesInRow e rxs (drop rx1 s))

fillSafeSpacesInCols :: Nono -> [[[Color]]]
fillSafeSpacesInCols (N c r cx rx col s) = rotateSolution (fillSafeSpacesInRows (N r c rx cx col (rotateSolution s)))

--solve the nonogram - main loop
tryForSolution :: Nono -> IO ()
tryForSolution (N c r cx rx col s) = do putStrLn("--------------------------------------------------------------------------------\nStarting new cycle of trying for solution.")
                                        let s1 = removeUnnecessaryColors (N c r cx rx col s)
                                        putStrLn("Solution after removing colors.")
                                        printGame (N c r cx rx col s1)
                                        let s2 = fillFullyFoundColors (N c r cx rx col s1)
                                        putStrLn("Solution after filling fully found colors.")
                                        printGame (N c r cx rx col s2)
                                        let s3 = processFirstCmds (N c r cx rx col s2)
                                        putStrLn("Solution after processing first and last cmd in each row and col.")
                                        printGame (N c r cx rx col s3)
                                        let s4 = processSingularCmds (N c r cx rx col s3)
                                        putStrLn("Solution after processing lines with only one cmd.")
                                        --putStrLn(show s4)
                                        printGame (N c r cx rx col s4)
                                        let col1 = removeCompletedColors (N c r cx rx col s4)
                                        putStr("Remaining color pallet:")
                                        putStrLn(show col1)
                                        if s4 /= s then tryForSolution (N c r cx rx col1 s4)
                                        else putStrLn("Solving completed.")

--solve the nonogram - main loop - first check, removing impossible Color placement
removeUnnecessaryColors :: Nono -> [[[Color]]]
removeUnnecessaryColors (N c r cx rx col s) = rotateSolution (removeUnnecessaryColorsFromRows cx col (rotateSolution (removeUnnecessaryColorsFromRows rx col s)))

removeUnnecessaryColorsFromRows :: [[(Int,Color)]] -> [Color] -> [[[Color]]] -> [[[Color]]]
removeUnnecessaryColorsFromRows [] _ _ = []
removeUnnecessaryColorsFromRows _ [] (s:ss) = [s]
removeUnnecessaryColorsFromRows (rx:rxs) (col:cols) (s:ss) = if col == White then (removeUnnecessaryColorsFromRows (rx:rxs) cols (s:ss)) ++ (removeUnnecessaryColorsFromRows rxs (col:cols) ss)
                                                             else do let n = sum [x1 | (x1,x2) <- rx, x2 == col]
                                                                     let ns = sum [1 | x<-s, x == [col]]
                                                                     if (n /= 0) && (n/=ns) then removeUnnecessaryColorsFromRows (rx:rxs) cols (s:ss)
                                                                     else if n == 0 then removeUnnecessaryColorsFromRows (rx:rxs) cols ([filter (/=col) x | x<-s]:ss)
                                                                          else removeUnnecessaryColorsFromRows (rx:rxs) cols (([if x == [col] then x else filter (/=col) x | x<-s]):ss)

--solve the nonogram - main loop - second check, fill color if all possible places found
fillFullyFoundColors :: Nono -> [[[Color]]]
fillFullyFoundColors (N c r cx rx col s) = fillFullyFoundColorsInSolution cx col (rotateSolution (fillFullyFoundColorsInRows cx col (rotateSolution (fillFullyFoundColorsInRows rx col s))))

fillFullyFoundColorsInSolution :: [[(Int,Color)]] -> [Color] -> [[[Color]]] -> [[[Color]]]
fillFullyFoundColorsInSolution _ [] s = s
fillFullyFoundColorsInSolution cx (col:cols) s = if col == White then fillFullyFoundColorsInSolution cx cols s
                                                  else do let n = (sum [sum [x1 | (x1,x2) <- x, x2 == col] | x <- cx])
                                                          let ns = sum [sum [length (filter (==col) y) | y<-x] | x<- s]
                                                          if n == ns then fillFullyFoundColorsInSolution cx cols [[if col `elem` y then [col] else y | y<-x] | x<-s]
                                                          else fillFullyFoundColorsInSolution cx cols s

fillFullyFoundColorsInRows :: [[(Int,Color)]] -> [Color] -> [[[Color]]] ->[[[Color]]]
fillFullyFoundColorsInRows [] _ _ = []
fillFullyFoundColorsInRows _ [] (s:ss) = [s]
fillFullyFoundColorsInRows (rx:rxs) (col:cols) (s:ss) = if col == White then (fillFullyFoundColorsInRows (rx:rxs) cols (s:ss)) ++ (fillFullyFoundColorsInRows rxs (col:cols) ss)
                                                        else do let n = sum [x1 | (x1,x2) <- rx, x2 == col]
                                                                let ns = sum [length (filter (==col) x) | x<-s]
                                                                if (n /= 0) && (n/=ns) then fillFullyFoundColorsInRows (rx:rxs) cols (s:ss)
                                                                else if n == 0 then fillFullyFoundColorsInRows (rx:rxs) cols (s:ss)
                                                                     else fillFullyFoundColorsInRows (rx:rxs) cols (([if col `elem` x then [col] else x | x<-s]):ss)


--solve the nonogram - main loop - third check, process first/last cmd in row/column
processFirstCmds :: Nono -> [[[Color]]]
processFirstCmds (N c r cx rx col s) = do let s1 = processFirstCmdInRows rx s
                                          let s2 = [reverse y | y <- (processFirstCmdInRows [reverse x | x <- rx] [reverse x | x <- s1])]
                                          let s3 = rotateSolution (processFirstCmdInRows cx (rotateSolution s2))
                                          rotateSolution ([reverse y | y <- (processFirstCmdInRows [reverse x | x <- cx] [reverse x | x <- (rotateSolution s3)])])

processFirstCmdInRows :: [[(Int,Color)]] -> [[[Color]]] -> [[[Color]]]
processFirstCmdInRows [] [] = []
processFirstCmdInRows (rx:rxs) (s:ss) = [processFirstCmdInRow (head rx) s] ++ processFirstCmdInRows rxs ss

processFirstCmdInRow :: (Int,Color) -> [[Color]] -> [[Color]]
processFirstCmdInRow (0,_) s = s
processFirstCmdInRow (rx1,rx2) (s:ss) = if s == [White] then [s] ++ processFirstCmdInRow (rx1,rx2) ss
                                        else if s == [rx2] then (take (rx1) (repeat [rx2])) ++ drop (rx1-1) ss
                                             else do let i = elemIndices [White] (s:ss)
                                                     let c = elemIndices [rx2] (s:ss)
                                                     if i == [] && c == [] then [filter (`elem` [White,rx2]) x | x<-take rx1 (s:ss)] ++ drop (rx1-1) ss
                                                     else if i /= [] && c == [] then if head i < rx1 then (take (head i) (repeat [White])) ++ processFirstCmdInRow (rx1,rx2) (drop (head i) (s:ss))
                                                                                     else [filter (`elem` [White,rx2]) x | x<-take rx1 (s:ss)] ++ drop (rx1-1) ss
                                                          else if i == [] && c /= [] then if head c >= rx1 then [filter (`elem` [White,rx2]) x | x<-take rx1 (s:ss)] ++ drop (rx1) (s:ss)
                                                                                          else if length c == 1 || last c < rx1 then [filter (`elem` [White,rx2]) x | x<-take (head c) (s:ss)] ++ (take (rx1 - head c) (repeat [rx2])) ++ drop (rx1) (s:ss)
                                                                                               else do let g = [x | x<-zipWith (-) (tail c) (init c)]
                                                                                                       if head g == 1 && (length (head (group g))) + head c >= rx1 then [[White]] ++ processFirstCmdInRow (rx1,rx2) ss
                                                                                                       else [filter (`elem` [White,rx2]) x | x<-take (head c) (s:ss)] ++ (take (rx1 - head c) (repeat [rx2])) ++ drop (rx1) (s:ss)
                                                               else if head i < head c then if head i < rx1 then (take (head i) (repeat [White])) ++ processFirstCmdInRow (rx1,rx2) (drop (head i) (s:ss))
                                                                                            else [filter (`elem` [White,rx2]) x | x<-take rx1 (s:ss)] ++ drop (rx1-1) ss
                                                                    else if head c >= rx1 then [filter (`elem` [White,rx2]) x | x<-take rx1 (s:ss)] ++ drop (rx1) (s:ss)
                                                                         else if length c == 1 || last c < rx1 then [filter (`elem` [White,rx2]) x | x<-take (head c) (s:ss)] ++ (take (rx1 - head c) (repeat [rx2])) ++ drop (rx1) (s:ss)
                                                                              else do let g = [x | x<-zipWith (-) (tail c) (init c)]
                                                                                      if head g == 1 && (length (head (group g))) + head c >= rx1 then [[White]] ++ processFirstCmdInRow (rx1,rx2) ss
                                                                                      else [filter (`elem` [White,rx2]) x | x<-take (head c) (s:ss)] ++ (take (rx1 - head c) (repeat [rx2])) ++ drop (rx1) (s:ss)

--solve the nonogram - main loop - fourth check, processing lines with only one cmd
processSingularCmds :: Nono -> [[[Color]]]
processSingularCmds (N c r cx rx col s) = rotateSolution (processSingularCmdsInRows cx (rotateSolution (processSingularCmdsInRows rx s)))

processSingularCmdsInRows :: [[(Int,Color)]] -> [[[Color]]] -> [[[Color]]]
processSingularCmdsInRows [] [] = []
processSingularCmdsInRows (rx:rxs) (s:ss) = if length rx /= 1 then [s] ++ processSingularCmdsInRows rxs ss
                                            else do let i = elemIndices ([snd (head rx)]) s
                                                    if i == [] then do let j = elemIndices ([White]) s
                                                                       if j == [] then [s] ++ processSingularCmdsInRows rxs ss
                                                                       else do let p = [head j] ++ (map (subtract 1) (zipWith (-) (tail j) (init j))) ++ [(length s) - (last j) - 1]
                                                                               if length (filter (>= fst (head rx)) p) /= 1 || maximum p >= 2*fst (head rx) then [s]++processSingularCmdsInRows rxs ss
                                                                               else do let m = elemIndices (maximum p) p
                                                                                       if m == [0] then [(take ((maximum p) - fst (head rx)) s) ++
                                                                                                         (take (2*(fst (head rx)) - maximum p) (repeat [snd (head rx)])) ++
                                                                                                         (take ((maximum p) - fst (head rx)) (drop (fst (head rx)) s)) ++
                                                                                                         (take ((length s) - maximum p) (repeat [White]))] ++ processSingularCmdsInRows rxs ss
                                                                                       else if m == [(length p) - 1] then [(take ((length s) - maximum p) (repeat [White])) ++
                                                                                                                           (take ((maximum p) - fst (head rx)) (drop ((length s) - maximum p) s)) ++
                                                                                                                           (take (2*(fst (head rx)) - maximum p) (repeat [snd (head rx)])) ++
                                                                                                                           (drop ((length s) - (maximum p) + fst (head rx)) s)] ++ processSingularCmdsInRows rxs ss
                                                                                            else [(take (1 + j !! ((head m)-1)) (repeat [White])) ++
                                                                                                  (take ((maximum p) - fst (head rx)) (drop (j !! ((head m)-1)) s)) ++
                                                                                                  (take (2*(fst (head rx)) - maximum p) (repeat [snd (head rx)])) ++
                                                                                                  (take ((maximum p) - fst (head rx)) (drop ((j !! ((head m)-1)) + fst (head rx)) s)) ++
                                                                                                  (take ((length s) - (j !! (head m))) (repeat [White]))] ++ processSingularCmdsInRows rxs ss
                                                    else do let n = 1 + (last i) - head i
                                                            [(take ((head i)-(fst (head rx)) + n) (repeat [White])) ++
                                                             (take ((fst (head rx))-n) (drop ((head i)-(fst (head rx)) + n) s)) ++ 
                                                             (take n (repeat [snd (head rx)])) ++
                                                             (take ((fst (head rx)) - n) (drop ((head i) + n) s)) ++
                                                             (take ((length s) - (fst (head rx)) - head i) (repeat [White]))] ++ processSingularCmdsInRows rxs ss

--solve the nonogram - main loop - color pallete update
removeCompletedColors :: Nono -> [Color]
removeCompletedColors (N _ _ _ _ [] _) = []
removeCompletedColors (N c r cx rx (col:cols) s) = if col == White then [col] ++ removeCompletedColors (N c r cx rx cols s)
                                                   else do let n = (sum [sum [x1 | (x1,x2) <- x, x2 == col] | x <- cx])
                                                           let ns = sum [sum [1 | y<-x,y==[col]] | x<- s]
                                                           if n == ns then removeCompletedColors (N c r cx rx cols s)
                                                           else [col] ++ removeCompletedColors (N c r cx rx cols s)