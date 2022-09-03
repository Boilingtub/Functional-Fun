import System.IO
import Data.List

-- ALLS		+ Numbers : unique
-- WELL		+ Numbers [1..9]
-- THAT         + Numbers are Int
-- ENDS		A*2 , D*1 , E*2 , H*1 , L*4 , N*1 , S*2 , T*2 , W*1
--------        (0)   (1)   (2)   (3)   (4)   (5)   (6)   (7)   (8)
-- SWELL	E*1 , L*2 , S*1 , W*1

wrapAround :: Int -> Int -> Int
wrapAround wrap inval = if outval > wrap then wrapAround wrap outval else outval
   where
    outval = if inval > wrap then inval - wrap else inval

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial(n-1) 


calcNFactorials :: Int -> [Int] -> [Int]
calcNFactorials inInt factlist = if inInt > -1 then calcNFactorials (inInt-1) newfactlist else factlist  
 where
  fact = factorial inInt
  newfactlist = factlist ++ [fact]


hasDuplicates :: (Ord a) => [a] -> Bool
hasDuplicates list = length (nub list) /= length list


appendList val [] = [val]
appendList val (x:xs) = x : appendList val xs



swapItemsAt :: Int -> Int -> [a] -> [a]
swapItemsAt a b list = let itemA = list!!a
                           itemB = list!!b
                           leftlist = take a list
                           middellist = take (b-a-1) (drop (a+1) list)
                           rightlist =  drop (b + 1) list
                           in leftlist ++ [itemB] ++ middellist ++ [itemA] ++ rightlist


incThoughList :: Int -> [Int] -> [Int]
incThoughList shiftval list = [wrapAround (length list) b | b <- addlist ]
    where
     addlist =  [x + shiftval | x <- list]


factorialList :: [Int]
factorialList = reverse (calcNFactorials 10 [])

getLargestDivisibleFactorial :: Int -> Int -> Int
getLargestDivisibleFactorial inval factToTest = if inval `mod` factorialList!!(factToTest) == 0 then factToTest
                                                else if factToTest > 1 then getLargestDivisibleFactorial inval (factToTest-1)
                                                else 1 

divLargestFactorial :: Int -> Int -> Int
divLargestFactorial inval factToTest = inval `div` (getLargestDivisibleFactorial inval factToTest)




orglist = [[1..4]]
getAllCombinationsOfList :: Int -> [[Int]] -> [[Int]]
getAllCombinationsOfList listCount lists = if maxCount-2 >= newCount
                                                   then getAllCombinationsOfList newCount finlists
                                                   else finlists
  where
    newCount = listCount+1
    maxCount = factorialList!!(length (lists!!0))
    lrgdivfact = getLargestDivisibleFactorial newCount (length (lists!!0))
    swapStart = divLargestFactorial newCount (length (lists!!0))
    swapEnd = lrgdivfact+1
    chglist = swapItemsAt (swapStart) (swapEnd) (last lists)
    finlists = appendList chglist lists


calcSwellBasic :: Int -> [Int]
calcSwellBasic iteration = if swell == alls + well + that + ends then calclist else [0] 
  where
   calclist = [1..9]
   alls  = calclist!!0 * 1000 + calclist!!4 * 100 + calclist!!4 * 10 + calclist!!6
   well  = calclist!!0 * 1000 + calclist!!2 * 100 + calclist!!4 * 10 + calclist!!4
   that  = calclist!!7 * 1000 + calclist!!3 * 100 + calclist!!0 * 10 + calclist!!7
   ends  = calclist!!2 * 1000 + calclist!!5 * 100 + calclist!!1 * 10 + calclist!!6
   swell = calclist!!6 * 10000 + calclist!!8 * 1000 + calclist!!2 * 100 + calclist!!4 * 10 + calclist!!4




main :: IO ()
main = do
        putStrLn "fun with haskell"
        let num = 24
        putStrLn ("start : " ++ show (divLargestFactorial num 9))
        putStrLn ("end : " ++ show (getLargestDivisibleFactorial num 9) )
        --let allCombinations = getAllCombinationsOfList 0 [[1,2,3,4]]
        --putStrLn(show(allCombinations))
        --putStrLn("has Duplicates : " ++ show(hasDuplicates(allCombinations)))
        --putStrLn (show (factorialList) )
