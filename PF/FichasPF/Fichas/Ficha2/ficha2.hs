

dobros :: [Float] -> [Float]
dobros [] = []
dobros (x:xs) = (2*x) : dobros xs