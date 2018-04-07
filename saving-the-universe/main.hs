import System.IO (isEOF)
import Text.Printf

-- Terrible naming...
-- countHacks' :: Integer -> Integer -> Integer -> Integer -> [Integer] -> Maybe Integer
countHacks' targetDmg curDmg curDmgPerS numHacks (bin:[])
    | curDmg <= targetDmg   = Just numHacks
    | otherwise             = Nothing
countHacks' targetDmg curDmg curDmgPerS numHacks (0:bins) = 
        countHacks' targetDmg curDmg (curDmgPerS `quot` 2) numHacks bins
countHacks' targetDmg curDmg curDmgPerS numHacks (bin:bin2:bins)
    | curDmg <= targetDmg   = Just numHacks
    | otherwise             = countHacks' targetDmg (curDmg-(curDmgPerS `quot` 2)) curDmgPerS (numHacks+1) (bin-1:bin2+1:bins)

countHacks :: Integer -> String -> Maybe Integer
countHacks d instrs = do
    let (dmg, dmgPerS) = calcDamage instrs 0 1
                    where 
                        calcDamage [] curDmg curDmgPerS = (curDmg, curDmgPerS)
                        calcDamage (x:xs) curDmg curDmgPerS 
                            | x == 'C' = calcDamage xs curDmg (curDmgPerS*2)
                            | x == 'S' = calcDamage xs (curDmg+curDmgPerS) curDmgPerS
    -- In descending order, lists the number of shots in each 'bin', each 'bin'
    -- represents half as much damage as the previous.
    let bins = formBins [0] instrs
            where 
                formBins bins [] = bins
                formBins (bin:bins) (x:xs)
                        | x == 'C' = formBins (0:bin:bins) xs
                        | x == 'S' = formBins ((bin+1):bins) xs
    countHacks' d dmg dmgPerS 0 bins
    
    
solve :: String -> Integer -> String
solve input lineNo = do
    let a : instrs = words input
    let num = countHacks (read a :: Integer) (head instrs)
    case num of 
        Nothing -> printf "Case #%d: IMPOSSIBLE" lineNo
        Just hacks -> printf "Case #%d: %d" lineNo hacks

myLoop :: Integer -> IO ()                   
myLoop i = do 
            done <- isEOF
            if done
                then return ()
                else do 
                        inp <- getLine
                        putStrLn $ solve inp i
                        myLoop $ i+1

    
main = do
        getLine
        myLoop 1