import BillBoard

prettyprint [] _ = []
prettyprint (h:l) i = ("Case #" ++ (show i) ++ ": " ++ (show h)): (prettyprint l (i+1))
                    
main =  do
    a <- getLine 
    result <- sequence (take (read a) (cycle [getLine]))
    mapM_ putStrLn (prettyprint (map solveboard result) 1)
