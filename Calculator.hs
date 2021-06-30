import System.IO
import Parser

-- UI display

ui :: [String]
ui = ["+---------------+",
      "|               |", 
      "+---+---+---+---+", 
      "| q | c | d | = |", 
      "+---+---+---+---+", 
      "| 1 | 2 | 3 | + |", 
      "+---+---+---+---+", 
      "| 4 | 5 | 6 | - |", 
      "+---+---+---+---+",
      "| 7 | 8 | 9 | * |",
      "+---+---+---+---+",
      "| 0 | ( | ) | / |",
      "+---+---+---+---+"]

sx :: Int -> Int
sx x = x + 50

sy :: Int -> Int
sy y = y + 10

cls :: IO()
cls = print "\ESC[2J"

type Pos = (Int,Int)

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeat :: Pos -> String -> IO ()
writeat p xs =  do goto p
                   putStr xs
                   hFlush stdout


disUI :: [String] -> IO()
disUI xss = sequence_ [writeat (sx 0,sy i) xs |(i,xs)<- zip [1..] xss]

wait :: Int -> IO ()
wait n = last [return()|_<-[2..n]]


-- Cal interaction
    -- list all posible buttons
buttons :: String
buttons = "0123456789()=+-*/qcd" ++ "\n\DEL"

getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

quit :: IO ()
quit = goto (1,sy 19)

clear :: IO()
clear = cal []

eval :: String -> IO()
eval xs = cal (show (evalP xs))

add :: Char -> String -> IO ()
add x xs = cal (xs ++ [x])

beep :: String -> IO ()
beep xs = do putStr "\BEL"
             cal xs

process :: Char -> String -> IO ()
process x xs | x == 'q'  = quit
             | x == 'c'  = clear
             | elem x "d\DEL"  = let nxs = (if length xs < 1 then [] else init xs) in cal nxs
             | elem x "=\n"  = eval xs
             | otherwise = add x xs


refresh :: IO()
refresh = writeat (sx 3,sy 2) (replicate 13 ' ')


display :: String -> IO()
display xs = writeat (sx 3,sy 2) (reverse (take 13 (reverse xs)))

cal :: String -> IO()
cal xs = refresh >> display xs >> getCh >>= \c ->
            if elem c buttons then
               process c xs
            else beep xs

main = do hSetBuffering stdin NoBuffering
          cls
          disUI ui
          hFlush stdout
          clear

