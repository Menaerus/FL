module BillBoard (solveone, solveboard) where 
-- Given a list of words and a line length       
-- returns the remaining list of words after greedily adding all possible
-- words to a line of that length
addline :: [[Char]] -> Int -> [[Char]]
addline [] _ = []
addline l 0 =  l
addline (w:t) n 
  | (length w) == n = t
  | (length w) < n = addline t (n - (length w) - 1)
  | otherwise = w:t


-- Given a list of words, a number of lines and a number of chars per line
-- returns true if text fits in that number of lines with that limit in line
-- size
testboard :: [[Char]] -> Int -> Int -> Bool
testboard [] _ _ = True
testboard (h:l) 0 _ = False
testboard l remaininglines charsperline = 
  let 
    newline = addline l charsperline
  in
    testboard newline (remaininglines -1) charsperline
         
-- Given a list of words, width, height and font size returns true iff
-- text fits in a billboard of that size (width and height) with given 
-- font size         
test ::  [[Char]] -> Int -> Int -> Int -> Bool                                          
test list width height fontsize = testboard list (div height fontsize) (div width fontsize)

-- Given a list of words, min font size, max font size, width and height
-- where max is not a valid font size and min is
-- returns a font size r, such that min <= r < max such that
-- for all s > r, s is not valid and r is valid
-- it performs at most log(max) recursive calls
binarysearch :: [[Char]] -> Int -> Int -> Int -> Int -> Int  
binarysearch list mini maxi width height 
  | mini == (maxi - 1)  = mini
  | (mini < (maxi - 1)) && (test list width height (div (mini+maxi) 2)) = binarysearch list (div (mini+maxi) 2) maxi width height          
  | (mini < (maxi - 1)) && not (test list width height (div (mini+maxi) 2)) = binarysearch list mini (div (mini+maxi) 2) width height
  | otherwise = 0


-- solves one of the billboard problems
-- given the text as string, width and height returns font size 
solveone :: [Char] -> Int -> Int -> Int
solveone [] _ _ = 0  
solveone text width height = binarysearch (words text) 0 ((minimum [width,height]) + 1) width height  

-- string version of solveone
solveboard l = solveone (unwords (drop 2 (words l))) (read (head (words l))) (read (head (tail (words l))))

