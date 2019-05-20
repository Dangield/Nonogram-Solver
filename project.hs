import System.Console.ANSI
import System.IO
import Data.List.Split
import Data.Char
import Data.List

data Nono = N Int Int [(Int,Int,Color)] [(Int,Int,Color)] deriving Show

--load game from file initialize solution and print everything
--usage: loadGame name_of_file_with_puzzle
loadGame :: String -> IO()
loadGame fname = do handle <- openFile fname ReadMode
                    contents <- hGetContents handle
                    let (c:r:x) = splitOn ";" contents
                    let n = N (read c :: Int) (read r :: Int) (enrichCmds (loadCols (splitOn "|" (head x)))) (enrichCmds (loadCols (splitOn "|" (head (tail x)))))
                    putStr(nonoToString n)
                    let s = initiateSolution (read c :: Int) (read r :: Int)
                    printGame n s

--initializes solution structure
--usage: initiateSolution num_of_cols num_of_rows
initiateSolution :: Int -> Int ->[[Int]]
initiateSolution c r = if r < 1 then []
                       else [initiateRow c] ++ initiateSolution c (r-1) 

--initializes one row of solution
--usage: initiateRow num_of_cols curr_col
initiateRow :: Int -> [Int]
initiateRow c= if c < 1 then []
                  else [8]++initiateRow (c-1)

--print whole game
--usage: printGame game solution
printGame :: Nono -> [[Int]] -> IO()
printGame (N c r x y) s = do let mc = countMaxNumOfCmd x
                             putStrLn("Max number of commands in column: " ++ show mc)
                             let mr = countMaxNumOfCmd y
                             putStrLn("Max number of commands in row: " ++ show mr)
                             printCols x 1 c mc mr
                             printRows y s 1 r mr

--prints cmds for cols
--usage: printCols col_cmds curr_col num_of_cols max_num_of_cmds_in_col max_num_of_cmds_in_row
printCols :: [(Int,Int,Color)] -> Int -> Int -> Int -> Int -> IO()
printCols _ _ _ 0 _ = putStr("")
printCols xs n c mc mr = if n > c+mr then do putStrLn("")
                                             printCols xs 1 c (mc-1) mr
                         else if n<= mr then do putStr(" ")
                                                printCols xs (n+1) c mc mr
                              else if (countNumOfCmd xs (n-mr)) < mc then do putStr("  ")
                                                                             printCols xs (n+1) c mc mr
                                   else do let (a,b,d) = head [(x1,x2,x3) | (x1,x2,x3) <- xs, x1==n-mr]
                                           if b<10 then putCharWithColor (intToDigit b) d
                                           else putCharWithColor (chr (b+87)) d
                                           putStr(" ")
                                           printCols (delete (a,b,d) xs) (n+1) c mc mr

--counts max num of cmds in any row/col
--usage: countMaxNumOfCmd cols/rows_cmds
countMaxNumOfCmd :: [(Int,Int,Color)] -> Int
countMaxNumOfCmd [(x,y,z)] = 1
countMaxNumOfCmd [(x,y,z),(xs,ys,zs)] = if x == xs then 2
                                        else 1
countMaxNumOfCmd ((x,y,z):(xs,ys,zs):xss) = if x == xs then
                                                if (length [(a,b,c)|(a,b,c)<-((x,y,z):(xs,ys,zs):xss),a==x]) == (length xss) + 2 then
                                                    (length xss) + 2
                                                else countMaxNumOfCmd ([(xs,ys,zs)] ++ xss ++ [(x,y,z)])
                                            else
                                                if (length [(a,b,c)|(a,b,c)<-((x,y,z):(xs,ys,zs):xss),a==x])>(length [(a,b,c)|(a,b,c)<-((x,y,z):(xs,ys,zs):xss),a==xs]) then
                                                    countMaxNumOfCmd ((x,y,z):xss)
                                                else countMaxNumOfCmd ((xs,ys,zs):xss)

--prints row cmds and solution
--usage: printRows row_cmds solution curr_row num_of_rows max_num_of_cmds_in_row
printRows :: [(Int,Int,Color)] -> [[Int]] -> Int -> Int -> Int -> IO()
printRows x s a r mr = if a<=r then do printRow x (head s) a mr (countNumOfCmd x a)
                                       printRows x (tail s) (a+1) r mr
                       else putStr("\n")

--prints single row
--usage: printRow row_cmds curr_row_solution curr_row_num max_num_of_cmds_in_row num_of_cmds_in_row
printRow :: [(Int,Int,Color)] -> [Int] -> Int -> Int -> Int -> IO()
printRow [] [] _ _ _ = putStrLn("")
printRow [] (s:ss) _ _ _ = if s == 8 then do putStr("_ ")
                                             printRow [] ss 0 0 0
                           else do putCharWithColor 'X' (toEnum s :: Color)
                                   putStr(" ")
                                   printRow [] ss 0 0 0
printRow ((a,b,c):xs) s r mr n = if a == r then
                                        if mr == n then do if b<10 then putCharWithColor (intToDigit b) c
                                                           else putCharWithColor (chr (b+87)) c
                                                           printRow xs s r mr n
                                        else do putStr(" ")
                                                printRow ((a,b,c):xs) s r mr (n+1)
                                 else if a > r then printRow [] s r mr n
                                      else printRow xs s r mr n

--count number of cmds in row/col
--usage: countNumOfCmd cmds row/col_num
countNumOfCmd :: [(Int,Int,Color)] -> Int -> Int
countNumOfCmd xs r = length [(a,b,c)|(a,b,c)<-xs,a==r]

--prints game to cmds list
--usage: nonoToString game
nonoToString :: Nono -> String
nonoToString (N _ _ [] []) = ""
nonoToString (N 0 0 [] ((a,b,c):xs)) = show a ++ "," ++ show b ++ "," ++ show c ++ "\n" ++ nonoToString (N 0 0 [] xs)
nonoToString (N 0 r [] y) = "Number of rows: " ++ show r ++ "\n" ++ nonoToString (N 0 0 [] y)
nonoToString (N 0 r ((a,b,c):xs) y) = show a ++ "," ++ show b ++ "," ++ show c ++ "\n" ++ nonoToString (N 0 r xs y)
nonoToString (N c r x y) = "Number of columns: " ++ show c ++ "\n" ++ nonoToString (N 0 r x y)

--loads cols/rows cmds from list of Strings
--usage: loadCols list_of_cols/rows_cmds_in_strings
loadCols :: [String] -> [(Int,Int,Color)]
loadCols []     = []
loadCols [x,y]  = [getCol(x),getCol(y)]
loadCols (s:ss) = [getCol(s)] ++ loadCols ss

--load single cmd from String
--usage: getCol cmd_as_String
getCol :: String -> (Int,Int,Color)
getCol s = do let x = splitOn "," s
              (read (head x) :: Int,read (head (tail x)) :: Int, toEnum(read (head (tail (tail x))) :: Int) :: Color)

--adds single White cmd between 2 cmds of the same Color in the same row/col
--usage: enrichCmds col/row_cmds
enrichCmds :: [(Int,Int,Color)] -> [(Int,Int,Color)]
enrichCmds [(x1,x2,x3)] = [(x1,x2,x3)]
enrichCmds ((x1,x2,x3):(xs1,xs2,xs3):xss) = if (x1==xs1 && x3==xs3) then [(x1,x2,x3),(x1,1,toEnum 7 :: Color)] ++ enrichCmds ((xs1,xs2,xs3):xss)
                                          else [(x1,x2,x3)] ++ enrichCmds ((xs1,xs2,xs3):xss)

--prints single char in chosen color
--usage: putCharWithColor char color
putCharWithColor :: Char -> Color -> IO ()
putCharWithColor x c = do
    setSGR [SetColor Foreground Vivid c]
    putChar x
    setSGR [Reset]
