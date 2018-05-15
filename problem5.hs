-- [ [row1] [row2] ...] in a row [col1, col2, ...]

row1 = [1, 2, 3, 4, 5] ::[Double]
row2 = [11, 21, 31, 41, 51] ::[Double]
row3 = [12, 22, 23, 42, 25] ::[Double]
matrix = [row1, row2, row3] :: [[Double]]

main =
  do 
    putStrLn("Row totals are " ++ show(s) ++ " Averages are " ++ show( avg s width) )
    putStrLn("Column totals are " ++ show(colSums) ++ " Averages are " ++ show( avg colSums height ) )
    where
    (head:tail) = matrix
    width = fromIntegral(length head)
    height = fromIntegral(length matrix)
    s = map sum matrix
    colSums = colTot matrix (repeat 0)
  
  
avg::[Double]->Double->[Double]
avg [] size = []
avg (n:ns) size = (n/size) : ns
  
  
rowAvg::[[Double]] -> IO()
rowAvg [] = return ()
rowAvg (r:part) =
  do 
    (amnt, total) <- rowHelp 0 0 r
    putStrLn("Total is " ++ show(total) ++ ". Avg is " ++ show(total/amnt))
    rowAvg part
  
  
  

rowHelp::Double->Double->[Double]->IO(Double,Double)
rowHelp amnt total [] = return(amnt, total)
rowHelp amnt total (x:xs) = 
  do
    (amnt, total) <- rowHelp (amnt+1) (total+x) xs
    return (amnt, total)
    
    
colTot::[[Double]] -> [Double] -> [Double]
colTot [] totals = totals
colTot (r1:rs) totals=
  colTot rs (zipWith (+) r1 totals)