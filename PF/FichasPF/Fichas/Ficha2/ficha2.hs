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
39.0
-}

-- (b)
funB :: [Int] -> [Int]
funB [] = []
funB (h:t) = if (mod h 2)==0 then h : (funB t)
                             else (funB t)

{-
mod h 2 -> resto da divisao de h por 2
se resto == 0, h é múltiplo  de 2, se resto /= 0, não é múltiplo

funB [8,5,12]               mod 8 2 = 0
8 : funB [5,12]             mod 5 2 = 1
8 : funB [12]               mod 12 2 = 0
8 : 12 : funB []
8 : 12 : []
[8,12]
-}





-- 2.

-- (a) Recebe uma lista e produz a lista em quecada elemento ´e o dobro do valor correspondente na lista de entrada.

dobros :: [Float] -> [Float]
dobros [] = []
dobros (x:xs) = (2*x) : dobros xs

-- (b) Calcula o número de vezes que um caracter ocorre numa string.
numOcorre :: Char -> String -> Int
numOcorre _ [] = 0
numOcorre c (x:xs)
                 | c == x = 1 + numOcorre c xs
                 | otherwise = numOcorre c xs

-- (c) Testa se uma lista só tem elementos positivos.
positivos :: [Int] -> Bool
positivos [] = True
positivos (i:is)
                | i >= 0 = True && positivos is
                | otherwise = False      -- ou False && positivos is ... como False && _ é sempre False, podemos escrever apenas False (é mais eficiente)

-- (d) Retira todos os elementos n˜ao positivos de uma lista de inteiros.
soPos :: [Int] -> [Int]
soPos [] = []
soPos (n:ns)
           | n >= 0 = n : soPos ns
           | otherwise = soPos ns