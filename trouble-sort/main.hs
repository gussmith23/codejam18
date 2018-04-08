import System.IO (isEOF)
import Text.Printf
import Data.List

takeEveryOther :: [a] -> [a]
takeEveryOther [] = []
takeEveryOther (x:[]) = [x]
takeEveryOther (x1:x2:xs) = x1 : (takeEveryOther xs)

interleave xs ys = concat (transpose [xs, ys])

solveAndFormat :: String -> Integer -> String
solveAndFormat input i = do
        let numsAsStrs = words input
        let evenList =  sort $ map read $ takeEveryOther numsAsStrs :: [Integer]
        let oddList = sort $ map read $ takeEveryOther (tail numsAsStrs) :: [Integer]
        let merged = interleave evenList oddList
        let errorLoc = findError merged 0 :: Maybe Integer
                where   findError [] _ = Nothing
                        findError (x:[]) _ = Nothing
                        findError (x1:x2:xs) i
                            | x1 <= x2 = findError (x2:xs) (i+1)
                            | otherwise = Just i
        case errorLoc of
            Nothing -> printf "Case #%d: OK" i
            Just errorLoc -> printf "Case #%d: %d" i errorLoc
        
main = do
    getLine
    handleCase 1
        where handleCase i = do
                eof <- isEOF
                if eof then return ()
                    else do getLine
                            input <- getLine
                            putStrLn $ solveAndFormat input i
                            handleCase (i+1)