addline [] _ = []
addline l 0 =  l
addline (w:t) n 
  | (length w) == n = t
  | (length w) < n = addline t (n - (length w) - 1)
  | otherwise = w:t

testboard [] _ _ = True
testboard (h:l) 0 _ = False
testboard l remaininglines charsperline = 
  let 
    newline = addline l charsperline
  in
    testboard newline (remaininglines -1) charsperline
                                            
test list width height fontsize = testboard list (div height fontsize) (div width fontsize)

binarysearch list mini maxi width height 
  | mini == (maxi - 1)  = mini
  | (mini < (maxi - 1)) && (test list width height (div (mini+maxi) 2)) = binarysearch list (div (mini+maxi) 2) maxi width height          
  | (mini < (maxi - 1)) && not (test list width height (div (mini+maxi) 2)) = binarysearch list mini (div (mini+maxi) 2) width height
  | otherwise = 0

solveone [] _ _ = 0  
solveone text width height = binarysearch (words text) 0 ((minimum [width,height]) + 1) width height  

