{-
1. Indique como ´e que o interpretador de Haskell avalia as express˜oes das al´ıneas que se
seguem, apresentando a cadeia de redu¸c˜ao de cada uma dessas express˜oes (i.e., os v´arios
passos interm´edios at´e se chegar ao valor final).
-}

-- (a)
funA :: [Double] -> Double
funA [] = 0
funA (y:ys) = y^2 + (funA ys)

{-
funA [2,3,5,1]
2^2 + funA [3,5,1]
2^2 + 3^2 + funA [5,1]
2^2 + 3^2 + 5^2 + funA [1]
2^2 + 3^2 + 5^2 + 1^2 + funA []
2^2 + 3^2 + 5^2 + 1^2 + 0
4+9+25+1+0
39
-}





-- 2.

-- (a)

dobros :: [Float] -> [Float]
dobros [] = []
dobros (x:xs) = (2*x) : dobros xs