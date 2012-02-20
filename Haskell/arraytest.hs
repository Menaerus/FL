import Data.Array

next a = a // [((1,1), True)]

--Next a = a // [ ( (i,j) , NextState (AliveNeighbours a i j iLo iHi jLo jHi) ) | i <- [iLo..iHi], j <- [jLo..jHi] ] 
--  where ((iLo,jLo), (iHi,jHi)) = bounds a
--        NextState x = (x >=2) and (x <= 3)
--        AliveNeighbours a i j iLo' iHi' jLo' jHi' = length (filter (id) [ a!(i',j') | i' <- [(max iLo' (i-1)).. (min iHi' (i+1))], j' <- [(max jLo' (j-1)).. (min jHi' (j+1))] ])
        
infiniteGame x = x 