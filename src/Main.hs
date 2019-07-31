module Main where
import System.Environment
import System.IO
import System.Random

--import Data.List.Split
--import Data.Set
--import Data.List

main = do 
 args <- getArgs
 if (length args) < 1
 then do
  putStrLn("Missing Length")
 else do
  let plen = (read (args!!0))::Int

  -- let mm = [[0.1, 0.2, 0.7], [0.2, 0.6, 0.2], [0.0, 0.0, 1.0]]
  let mm = [[0.1, 0.2, 0.7], [0.2, 0.6, 0.2], [0.4, 0.4, 0.2]]

  genMarkovWalk plen mm


genMarkovWalk p m =
 genMarkovWalk' 1 p 0 m

genMarkovWalk' a n s m  =
 if a > n then do
  return ()
 else do
   r <- getRandom01
   let s' = getNextState 0 (length (m!!s)) 0.0 r (m!!s)
   putStrLn("Transition " ++ (show a) ++ 
             " State " ++ (show s) ++ " To " ++ (show s'))
   genMarkovWalk' (a+1) n s' m


genStates n m = 
 genStates' 0 n m

genStates' :: Int -> Int -> [Float] -> IO ()
genStates' a n m =
  if a == n then do
   return ()
  else do
   r <- getRandom01
   let s  = getNextState 0 (length m) 0.0 r m
   putStrLn("r: " ++ (show r) ++ ", m: " ++ (show m) ++ ", s: " ++ (show s))
   genStates' (a+1) n m

getNextState _ n _ _ [] = n
getNextState a n z r m =
 let h = ((head m) + z) in
  if r < h then a
  else getNextState (a+1) n h r (tail m) 

getRandom01 = do
 randomRIO(0,1 :: Float)

