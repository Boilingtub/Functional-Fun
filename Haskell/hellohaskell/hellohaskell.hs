import Data.List
import System.IO


name = "Jan-Hendrik"

minInt = minBound :: Int
maxInt = maxBound :: Int

pos1 = [3,3]
pos2 = [4,4]

calcDistance :: (Float , Float) -> (Float , Float) -> Float
calcDistance (x1 , y1) (x2 , y2) = sqrt (dx*dx + dy*dy)
 where
  dx = x1 - x2
  dy = y1 - y2
 

main :: IO ()
main = do
         putStrLn ( "Hello My name is " ++ name )

         putStrLn ("")

         putStrLn ("I can count up to " ++ show(maxInt) )
         putStrLn ("And down to " ++ show(minInt) )

         putStrLn ("")

         putStrLn ("I am standing at" ++ show(pos1) )
         putStrLn ("And I am going to" ++ show(pos2) )
         putStrLn ("I am Going to Walk " ++ show(calcDistance (pos1!!0 , pos1!!1) (pos2!!0 , pos2!!1)) ++ " Distance")

         return ()
