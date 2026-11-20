data Hora = H Int Int
    deriving Show


type Etapa = (Hora,Hora)
type Viagem = [Etapa]


-- [(H 9 30, H 10 25), (H 11 20, H 12 45), (H 13 30, H 14 45)]


-- 1)

-- a) Testar se uma etapa est´a bem constru´ıda (i.e., o tempo de chegada ´e superior ao de partida e as horas s˜ao v´alidas).

testaEtapa :: Etapa -> Bool
testaEtapa (saida, chegada) = validaHora saida && validaHora chegada && validaTempos saida chegada 


validaHora :: Hora -> Bool
validaHora (H h1 m1)
                   | h1>=0 && h1<24 && m1>=0 && m1<60 = True
                   | otherwise = False

validaTempos :: Hora -> Hora -> Bool
validaTempos (H h1 m1) (H h2 m2)
                                | h1 > h2 = False
                                | h1 == h2 && m1>m2 = False 
                                | otherwise = True




{-
4. Pretende-se guardar informa¸c˜ao sobre os anivers´arios das pessoas numa tabela que
associa o nome de cada pessoa `a sua data de nascimento. Para isso, declarou-se a
seguinte estrutura de dados
-}

type Dia = Int
type Mes = Int
type Ano = Int
type Nome = String

data Data = D Dia Mes Ano
      deriving Show

type TabDN = [(Nome,Data)]

-- a. Defina a função procura :: Nome -> TabDN -> Maybe Data, que indica a data de nascimento de uma dada pessoa, caso o seu nome exista na tabela.

procura :: Nome -> TabDN -> Maybe Data
procura _ [] = Nothing
procura nome ((n,d):h)
                      | nome == n = Just d
                      | otherwise = procura nome h

-- procura nome ((n, D d m a))








-- Queremos, caso exista no registo, devolver o ano do aniversário da pessoa


-- data Data = D Dia Mes Ano
 -- d (D dia mes ano)

procura1 :: Nome -> TabDN -> Maybe Ano
procura1 nome [] = Nothing
procura1 nome ((n, D dia mes ano):h)
                      | nome == n = Just ano 
                      | otherwise = procura1 nome h


























existe :: Nome -> TabDN -> Bool
existe _ [] = False
existe nome ((n,d):h)
                      | nome == n = True
                      | otherwise = existe nome h

-- b. Defina a função idade :: Data -> Nome -> TabDN -> Maybe Int, que calcula a idade de uma pessoa numa dada data.

idade :: Data -> Nome -> TabDN -> Maybe Int
idade date nome tabela
                      | validaData date && (existe nome tabela)  = calculaIdade date (procura nome tabela)
                      | otherwise = Nothing


calculaIdade :: Data -> Maybe Data -> Maybe Int
calculaIdade (D d1 m1 a1) (Just (D d2 m2 a2))
                                             | (a1-a2) >= 0 && m1 > m2 = Just (a1-a2)
                                             | (a1-a2) >= 0 && m1 == m2 && d1>d2 = Just (a1-a2)
                                             | (a1-a2) > 0 && m1 == m2 && d1<=d2 = Just (a1-a2-1)
                                             | otherwise = Nothing


validaData :: Data -> Bool
validaData (D dia mes ano)
                         | mes == 1 && (dia <= 0 || dia > 31) = False
                         | mes == 2 && (dia <= 0 || dia > 29) = False
                         | mes == 3 && (dia <= 0 || dia > 31) = False
                         | mes == 4 && (dia <= 0 || dia > 30) = False
                         | mes == 5 && (dia <= 0 || dia > 31) = False
                         | mes == 6 && (dia <= 0 || dia > 30) = False
                         | mes == 7 && (dia <= 0 || dia > 31) = False
                         | mes == 8 && (dia <= 0 || dia > 31) = False
                         | mes == 9 && (dia <= 0 || dia > 30) = False
                         | mes == 10 && (dia <= 0 || dia > 31) = False
                         | mes == 11 && (dia <= 0 || dia > 30) = False
                         | mes == 12 && (dia <= 0 || dia > 31) = False
                         | otherwise = True



-- (c)
anterior :: Data -> Data -> Bool
anterior (D d1 m1 a1) (D d2 m2 a2)
                                 | a1 < a2 = True
                                 | a1 == a2 && m1 < m2 = True
                                 | a1 == a2 && m1 == m2 && d1 < d2 = True
                                 | otherwise = False



{-
5. 
Considere o seguinte tipo de dados que descreve a informa¸c˜ao de um extracto banc´ario.
Cada valor deste tipo indica o saldo inicial e uma lista de movimentos. Cada movimento
´e representado por um triplo que indica a data da opera¸c˜ao, a sua descri¸c˜ao e a quantia
movimentada (em que os valores s˜ao sempre n´umeros positivos).
-}

data Movimento = Credito Float | Debito Float
                deriving Show

--data Data = D Int Int Int
  --    deriving Show
data Extracto = Ext Float [(Data, String, Movimento)]
         deriving Show


-- (a)
extValor :: Extracto -> Float -> [Movimento]
extValor (Ext inicial ((_,_,mov):h)) min
                                        | valorMov mov >= min = mov : extValor (Ext inicial h) min
                                        | otherwise = extValor (Ext inicial h) min

valorMov :: Movimento -> Float
valorMov (Credito valor) = valor
valorMov (Debito valor) = valor