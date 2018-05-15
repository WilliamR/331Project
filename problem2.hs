import System.IO
import System.Random
import Data.Ord
main = 
  do putStrLn "Enter 1(bike) 2(car) 3(plane):"
     g <- newStdGen
     command <- getLine
     case command of
       '1': entry -> bike 0 0 0 g
       '2': entry -> car 0 0 0 g
       '3': entry -> plane 0 0 0 g
    --putStrLn(read g)
   --bike 0 0 0 g
   --car 0 0 0 g
   --plane 0 0 0 g
    
bike:: RandomGen r => Int -> Int -> Int -> r -> IO ()
bike miles hours maxi gen=
  if(miles >= 1000)
  then
    do putStrLn ("The number of hours it took to travel 1000 miles was "++ (show hours) ++" hours.")
       putStrLn ("The maximum speed was " ++ (show maxi) ++ "mph.")
  else
      bike (miles + spd) (hours + 1) (max maxi spd) newgen
    where
    (spd, newgen) = randomR (5, 15) gen
    --fspd = 10*spd + 5

      
car:: RandomGen r => Int -> Int -> Int -> r -> IO ()
car miles hours maxi gen=
  if(miles >= 1000)
  then
    do putStrLn ("The number of hours it took to travel 1000 miles was "++ (show hours) ++" hours.")
       putStrLn ("The maximum speed was " ++ (show maxi) ++ "mph.")
  else
    car (miles + spd) (hours + 1) (max maxi spd) newgen
    where
    (spd, newgen) = randomR (20, 70) gen
  
  
plane:: RandomGen r => Int -> Int -> Int -> r -> IO ()
plane miles hours maxi gen=   
  if(miles >= 1000)
  then
    do putStrLn ("The number of hours it took to travel 1000 miles was "++ (show hours) ++" hours.")
       putStrLn ("The maximum speed was " ++ (show maxi) ++ "mph.")
  else
    plane (miles + spd) (hours + 1) (max maxi spd) newgen
    where
    (spd, newgen) = randomR (400, 600) gen
    
    
grand :: RandomGen g => g -> (Int, g)
grand gen = let (a, gen') = random gen
            in (a, gen')