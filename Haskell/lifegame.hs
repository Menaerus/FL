import Data.Array

-- returns a list of ints around [i-1..i+1] with exceptions in the margins, 
--    where a circular policy is applied 
myrange :: Int -> Int -> Int -> [Int]
myrange i low high  
    | i > low && i < high = [i-1..i+1]                                                                                       
    | i == low && (i+1) < high = [low..i+1]++[high]
    | i == low && (i+1) == high = [low..high]
    | (i-1) > low  && i == high = [low]++[i-1 .. high]  
    | (i-1) == low  && i == high = [low..high]
    | otherwise = []                    

-- list of neighbours for a given cell, with bounds as b
neighbours :: Int -> Int -> ((Int,Int), (Int,Int)) -> [(Int,Int)]
neighbours i j b = filter (/= (i,j)) [ (i',j') | i' <- myrange i ilo ihi , j' <- myrange j jlo jhi]
  where ((ilo, jlo), (ihi, jhi)) = b  

-- number of alive neighbours
aliveNeighbours :: Array(Int, Int) Bool -> Int -> Int -> Int
aliveNeighbours a i j = length (filter (id) [ a!(i',j') | (i',j') <- (neighbours i j (bounds a)) ])

-- next configuration of the matrix...
next :: Array(Int, Int) Bool -> Array(Int, Int) Bool
next a = a // [ ( (i,j) , nextState (aliveNeighbours a i j) $ a!(i,j) ) | i <- [iLo..iHi], j <- [jLo..jHi] ] 
  where ((iLo,jLo), (iHi,jHi)) = bounds a
        nextState x b = (b && (x == 2)) ||  (x == 3)

-- the infinite list of all Life worlds        
infiniteGame :: Array(Int, Int) Bool  -> [Array(Int, Int) Bool]      
infiniteGame x = x : (infiniteGame (next x))