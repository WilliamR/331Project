import Data.Char
import Data.List

array = ["This", "Is", "An", "Array"] ::[String]

main = 
  do printList (sortBy Main.compare array)
     
  
  

compare::String->String->Ordering
compare a b = Prelude.compare (Main.sum a) (Main.sum b)

sum::String->Int
sum [] = 0
sum (x:xs) = ord x + Main.sum xs

printList::[String]->IO()
printList [] = return()
printList (x:xs) =
  do putStrLn x
     printList xs