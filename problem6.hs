--Problem 6
import Data.Time.Clock
--G(n) = 1 + G(n - G( G( n - 1)))

main = 
  do 
     time <- getCurrentTime
     putStrLn (show (g 50))
     time2 <- getCurrentTime
     putStrLn( show (diffUTCTime time2 time))

g::Int->Int
g 1 = 1
g n =
  1 + g (n - g( g(n-1)))

  
showGn:: Int -> IO()
showGn amnt =
  do 
     time <- getCurrentTime
     putStrLn (show (g amnt))
     time2 <- getCurrentTime
     putStrLn( show (diffUTCTime time2 time))
