{-
Trabalho 2
Guilherme Teixeira - 49021
-}
import Data.Char

-- Devolve o Char equivalente dado quantas vezes o numero dois foi apertado
numeroDois :: Int -> [Char]
numeroDois 0 = "C"
numeroDois 1 = "A"
numeroDois 2 = "B"

-- Devolve o Char equivalente dado quantas vezes o numero tres foi apertado
numeroTres :: Int -> [Char]
numeroTres 0 = "F"
numeroTres 1 = "D"
numeroTres 2 = "E"

-- Devolve o Char equivalente dado quantas vezes o numero quarto foi apertado
numeroQuatro :: Int -> [Char]
numeroQuatro 0 = "I"
numeroQuatro 2 = "H"
numeroQuatro 1 = "G"

-- Devolve o Char equivalente dado quantas vezes o numero cinco foi apertado
numeroCinco :: Int -> [Char]
numeroCinco 0 = "L"
numeroCinco 2 = "K"
numeroCinco 1 = "J"

-- Devolve o Char equivalente dado quantas vezes o numero seis foi apertado
numeroSeis :: Int -> [Char]
numeroSeis 0 = "O"
numeroSeis 2 = "N"
numeroSeis 1 = "M"

-- Devolve o Char equivalente dado quantas vezes o numero sete foi apertado
numeroSete :: Int -> [Char]
numeroSete 0 = "S"
numeroSete 3 = "R"
numeroSete 2 = "Q"
numeroSete 1 = "P"

-- Devolve o Char equivalente dado quantas vezes o numero oito foi apertado
numeroOito :: Int -> [Char]
numeroOito 0 = "V"
numeroOito 2 = "U"
numeroOito 1 = "T"

-- Devolve o Char equivalente dado quantas vezes o numero nove foi apertado
numeroNove :: Int -> [Char]
numeroNove 0 = "Z"
numeroNove 3 = "Y"
numeroNove 2 = "X"
numeroNove 1 = "W"

-- Dado um numero em char apertado x vezes devolve a sua letra equivalente
charEquivalente :: Char -> Int -> [Char]
charEquivalente y x = case y of '2' -> numeroDois (x `mod` 3)
                                '3' -> numeroTres (x `mod` 3)
                                '4' -> numeroQuatro (x `mod` 3)
                                '5' -> numeroCinco (x `mod` 3)
                                '6' -> numeroSeis (x `mod` 3)
                                '7' -> numeroSete (x `mod` 4)
                                '8' -> numeroOito (x `mod` 3)
                                '9' -> numeroNove (x `mod` 4)
                                ' ' -> ""

-- Dado uma lista de char com o numero apertado devolve sua letra correspondente
listaParaLista :: [Char] -> [Char]
listaParaLista xs = case xs of [] -> ""
                               xs -> charEquivalente (head xs) (length xs)



-- Faz uma lista de chars seguidos que se encontram na primeira lista que ocorrem seguidamente na segunda lista
iguaisSeguidos :: [Char] -> [Char] -> [Char]
iguaisSeguidos [] [] = ""
iguaisSeguidos [] [x] = [x]
iguaisSeguidos [] (x:xs) = iguaisSeguidos [x] xs
iguaisSeguidos (y:ys) [] = (y:ys)
iguaisSeguidos (y:ys) (x:xs)
    | y /= x = (y:ys)
    | y == x = iguaisSeguidos ((y:ys) ++ [x]) (xs)

-- Dada uma lista de char devolve uma lista de strings que ocorrem sucessivamente
listaString :: [Char] -> [[Char]]
listaString [] = [""]
listaString [x] = [[x]]
listaString xs = j ++ listaString (drop u xs)
        where j = [iguaisSeguidos [] xs]
              u = length (j!!0)

-- Dada uma lista com os padroes digitados devolve a lista resultante de chars
listaPronta :: [String] -> [Char]
listaPronta [[]] = ""
listaPronta [x] = listaParaLista x
listaPronta xs = listaParaLista (xs!!0) ++ listaPronta (drop 1 xs)

-- TECLAS PARA PALAVRA
-- Dadas as teclas de [2..9] apertadas mostram as letras que sairiam em um telefone
teclasParaPalavra :: [Char] -> [Char]
teclasParaPalavra x = listaPronta (listaString x)
