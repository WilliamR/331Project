import System.IO
import System.Random
--import Control.Conditional
--import Control.Monad

main = 
  do
    g <- newStdGen
    (total, amnt) <- randLoop g 0 0
    --hFlush(stdout)
    putStrLn("\n"++ show(amnt) ++ " random numbers generated")
    putStrLn("With an average of " ++ show(total / amnt))
    
randLoop::RandomGen r => r->Double->Double->IO(Double,Double)
randLoop gen total recur = 
  do
    putStrLn("Start")
    has <- hWaitForInput stdin 1000
  --putStrLn(show(has))
  --let (x:xs) = line
    if(has == False)
    then
      do putStrLn(show (recur+1))
         randLoop newGen (total + add) (recur + 1)
    else
      return (total, recur)
    where
      (add, newGen) = randomR (0, 1) gen