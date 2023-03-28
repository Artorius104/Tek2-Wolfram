module Main where

import Lib
import System.Environment
import System.Exit
import System.IO
import Data.List

data Setup = Sep {
    rule :: String,
    start :: Int,
    line :: Int,
    window :: Int,
    move :: Int
} deriving Show

doRuleThree :: Setup -> Int -> Int -> String -> Int -> String -> String
doRuleThree sep n x str pos new_str | pos == 0 = createLine sep n (x + 1) str (pos + 1) (new_str ++ " ")
    | str !! pos == '*' && str !! (pos + 1) == '*' && str !! (pos - 1) == '*' = createLine sep n (x + 1) str (pos + 1) (new_str ++ " ")
    | str !! pos == '*' && str !! (pos + 1) == ' ' && str !! (pos - 1) == '*' = createLine sep n (x + 1) str (pos + 1) (new_str ++ "*")
    | str !! pos == ' ' && str !! (pos + 1) == '*' && str !! (pos - 1) == '*' = createLine sep n (x + 1) str (pos + 1) (new_str ++ "*")
    | str !! pos == ' ' && str !! (pos + 1) == ' ' && str !! (pos - 1) == '*' = createLine sep n (x + 1) str (pos + 1) (new_str ++ " ")
    | str !! pos == '*' && str !! (pos + 1) == '*' && str !! (pos - 1) == ' ' = createLine sep n (x + 1) str (pos + 1) (new_str ++ "*")
    | str !! pos == '*' && str !! (pos + 1) == ' ' && str !! (pos - 1) == ' ' = createLine sep n (x + 1) str (pos + 1) (new_str ++ "*")
    | str !! pos == ' ' && str !! (pos + 1) == '*' && str !! (pos - 1) == ' ' = createLine sep n (x + 1) str (pos + 1) (new_str ++ "*")
    | str !! pos == ' ' && str !! (pos + 1) == ' ' && str !! (pos - 1) == ' ' = createLine sep n (x + 1) str (pos + 1) (new_str ++ " ")
    | otherwise = createLine sep n (x + 1) str (pos + 1) (new_str ++ "*")

doRuleTwo :: Setup -> Int -> Int -> String -> Int -> String -> String
doRuleTwo sep n x str pos new_str | pos == 0 = createLine sep n (x + 1) str (pos + 1) (new_str ++ " ")
    | str !! pos == '*' && str !! (pos + 1) == '*' && str !! (pos - 1) == '*' = createLine sep n (x + 1) str (pos + 1) (new_str ++ " ")
    | str !! pos == '*' && str !! (pos + 1) == ' ' && str !! (pos - 1) == '*' = createLine sep n (x + 1) str (pos + 1) (new_str ++ "*")
    | str !! pos == ' ' && str !! (pos + 1) == '*' && str !! (pos - 1) == '*' = createLine sep n (x + 1) str (pos + 1) (new_str ++ " ")
    | str !! pos == ' ' && str !! (pos + 1) == ' ' && str !! (pos - 1) == '*' = createLine sep n (x + 1) str (pos + 1) (new_str ++ "*")
    | str !! pos == '*' && str !! (pos + 1) == '*' && str !! (pos - 1) == ' ' = createLine sep n (x + 1) str (pos + 1) (new_str ++ "*")
    | str !! pos == '*' && str !! (pos + 1) == ' ' && str !! (pos - 1) == ' ' = createLine sep n (x + 1) str (pos + 1)  (new_str ++ " ")
    | str !! pos == ' ' && str !! (pos + 1) == '*' && str !! (pos - 1) == ' ' = createLine sep n (x + 1) str (pos + 1) (new_str ++ "*")
    | str !! pos == ' ' && str !! (pos + 1) == ' ' && str !! (pos - 1) == ' ' = createLine sep n (x + 1) str (pos + 1) (new_str ++ " ")
    | otherwise = createLine sep n (x + 1) str (pos + 1) (new_str ++ "*")

doRuleOne :: Setup -> Int -> Int -> String -> Int -> String -> String
doRuleOne sep n x str pos new_str | pos == 0 = createLine sep n (x + 1) str (pos + 1) (new_str ++ " ")
    | str !! pos == '*' && str !! (pos + 1) == '*' && str !! (pos - 1) == '*' = createLine sep n (x + 1) str (pos + 1) (new_str ++ " ")
    | str !! pos == '*' && str !! (pos + 1) == ' ' && str !! (pos - 1) == '*' = createLine sep n (x + 1) str (pos + 1) (new_str ++ " ")
    | str !! pos == ' ' && str !! (pos + 1) == ' ' && str !! (pos - 1) == ' ' = createLine sep n (x + 1) str (pos + 1) (new_str ++ " ")
    | str !! pos == ' ' && str !! (pos + 1) == '*' && str !! (pos - 1) == ' ' = createLine sep n (x + 1) str (pos + 1) (new_str ++ "*")
    | str !! pos == '*' && str !! (pos + 1) == ' ' && str !! (pos - 1) == '*' = createLine sep n (x + 1) str (pos + 1) (new_str ++ "*")
    | str !! pos == '*' && str !! (pos + 1) == '*' && str !! (pos - 1) == '*' = createLine sep n (x + 1) str (pos + 1) (new_str ++ "*")
    | str !! pos == ' ' && str !! (pos + 1) == ' ' && str !! (pos - 1) == '*' = createLine sep n (x + 1) str (pos + 1) (new_str ++ "*")
    | str !! pos == ' ' && str !! (pos + 1) == '*' && str !! (pos - 1) == '*' = createLine sep n (x + 1) str (pos + 1) (new_str ++ " ")
    | otherwise = createLine sep n (x + 1) str (pos + 1) (new_str ++ "*")

createLine :: Setup -> Int -> Int -> String -> Int -> String -> String
createLine sep n x str pos new_str | x < 380 && rule sep == "30" = doRuleOne sep n x str pos new_str
                                    | x < 380 && rule sep == "90" = doRuleTwo sep n x str pos new_str
                                    | x < 380 && rule sep == "110" = doRuleThree sep n x str pos new_str
                                    | x == 380 = createLine sep n (x + 1) str (pos + 1) (new_str ++ " ")
                                    | otherwise = new_str

printLine :: Setup -> Int -> String -> IO ()
printLine sep n str | n < start sep - 1 = do
                        startAlgo sep (n + 1) (createLine sep n (-300) str 0 [])
                    | otherwise = do
                        putStrLn (take (window sep) (drop 300 (createLine sep n (-300) str 0 [])))
                        startAlgo sep (n + 1) (createLine sep n (-300) str 0 [])

startAlgo :: Setup -> Int -> String -> IO ()
startAlgo sep n str | n < (line sep + start sep) = printLine sep n str
                    | otherwise = return ()

placeStart :: Setup -> Int -> String -> String
placeStart sep n str | n == (40 + move sep) = placeStart sep (n + 1) (str ++ "*")
                    | n < (381 + move sep) = placeStart sep (n + 1) (str ++ " ")
                    | otherwise = str

firstLine :: Setup -> IO ()
firstLine sep = putStrLn (take (window sep) (drop 300 (placeStart sep (-300) [])))

wolfram :: Setup -> [String] -> Int -> IO ()
wolfram sep args n | start sep == 0 = do
                        firstLine sep
                        startAlgo sep 0 (placeStart sep (-300) [])
                    | otherwise = startAlgo sep 0 (placeStart sep (-300) [])

myExit :: IO a
myExit = hPutStrLn stderr "Error : wrong input" >> exitFailure

isInteger s = case reads s :: [(Integer, String)] of
    [(_, "")] -> True
    _         -> False

getRule :: [String] -> Int -> String
getRule args n | args !! n == "--rule" = args !! (n + 1)
                | otherwise = getRule args (n + 1)

getMove :: [String] -> Int -> Int
getMove args n | args !! n == "--move" = read (args !! (n + 1))
                | (n + 1) == length args = 0
                | otherwise = getMove args (n + 1)

getWindow :: [String] -> Int -> Int
getWindow args n | args !! n == "--window" = read (args !! (n + 1))
                | (n + 1) == length args = 80
                | otherwise = getWindow args (n + 1)

getLineLimit :: [String] -> Int -> Int
getLineLimit args n | args !! n == "--lines" = read (args !! (n + 1))
                    | (n + 1) == length args = 0
                    | otherwise = getLineLimit args (n + 1)

getStart :: [String] -> Int -> Int
getStart args n | args !! n == "--start" = read (args !! (n + 1))
                | (n + 1) == length args = 0
                | otherwise = getStart args (n + 1)

checkArgs :: [String] -> Int -> Int -> IO ()
checkArgs args n x | null args = myExit
                | ((args !! n) /= "--rule")
                    && ((args !! n) /= "--start")
                    && ((args !! n) /= "--lines")
                    && ((args !! n) /= "--window")
                    && ((args !! n) /= "--move")
                    && not (isInteger (args !! n)) = myExit
                | (args !! n) == "--rule"
                    && ((args !! (n + 1) == "30")
                    || (args !! (n + 1) == "90")
                    || (args !! (n + 1) == "110")) = checkArgs args (n + 1) (x + 1)
                |  (n + 1) == length args
                    && (x == 1) = return ()
                |  (n + 1) == length args
                    && (x /= 1) = myExit
                | otherwise = checkArgs args (n + 1) x

-- checkArgs :: [String] ->X Int -> Int -> IO ()
-- checkArgs ("--rule":xs) = 
-- checkArgs ("--rule":xs) = 
-- checkArgs ("--rule":xs) = 

main :: IO ()
main = do
    args <- getArgs
    checkArgs args 0 0
    let setup = Sep {rule = getRule args 0, start = getStart args 0, line = getLineLimit args 0 - 1, window = getWindow args 0, move = getMove args 0}
    wolfram setup args 0