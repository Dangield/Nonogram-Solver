import System.Console.ANSI
import System.IO
import Data.List.Split
import Data.Char
import Data.List

data Nono = N Int Int [[(Int,Color)]] [[(Int,Color)]] [[[Color]]] deriving Show

solve :: String -> IO ()
solve fname = do handle <- openFile fname ReadMode
                 contents <- hGetContents handle
                 let n = getNonoFromString contents
                 printGame n

--retrieving nonogram from file
getNonoFromString :: String -> Nono
getNonoFromString s = do let args = splitOn ";" s
                         let c = read (args !! 0) :: Int
                         let r = read (args !! 1) :: Int
                         let cx = getCmdsFromStrings (splitOn "|" (args !! 2))
                         let rx = getCmdsFromStrings (splitOn "|" (args !! 3))
                         let col = [White] ++ getColorsInNono (cx ++ rx)
                         N c r (enrichCmds (splitCmds cx)) (enrichCmds (splitCmds rx)) (initSolution c r col)

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
printGame (N c r cx rx s) = do let mc = maximum ([length x| x<-cx])
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

