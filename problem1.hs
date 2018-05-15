import System.IO
import Data.List

fname = "words.txt"
amount = 10
main = 	doRead fname amount
		

doRead::String->Int->IO()
doRead filename n =
  do contents <- readFile filename
     let wlist = words contents
     printList (nub wlist) 0 n
     
     
     
printList::[String]->Int->Int->IO()
printList [] recur n = return()
printList (x:xs) recur n =
  if(recur < n)
  then
    do putStrLn x
       printList xs (recur+1) n
  else
    return ()


