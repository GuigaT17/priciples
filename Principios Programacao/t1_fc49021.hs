{-
Trabalho 1
Guilherme Teixeira - 49021
-}

import Data.Char

-- Metodo que transforma uma string para uma lista de inteiros a seu numero equivalente na tabela ascii
letrasParaNumeros st = [ord y | y <- st]

-- Metodo que adiciona x a todo os numeros em uma lista de numeros inteiros
adicionaALista x ls = [y + x | y <- ls]

-- Metodo que coloca todos os numeros entre  o intevalo 0 e 127
rotacao ls = [x `mod` 128 | x <- ls]
rotacao ls = [if x < 0 then (x + 128) else x | x <- ls]

-- Metodo que transforma uma lista de numeros para letras correspondentes na tabela ascii
numerosParaLetras ls = [chr x | x <- ls]

-- CIFRAR E DECIFRAR CESAR
cifrarCesar x st = numerosParaLetras (rotacao (adicionaALista x (letrasParaNumeros st)))
decifrarCesar x st = numerosParaLetras (rotacao (adicionaALista (-x) (letrasParaNumeros st)))

-- Devolve uma lista de inteiros que vai de [0 .. length ls - 1]
listaAdicao ls = [x | x <- [0 .. length ls - 1]]

-- Zippa duas listas
listaZip ls = zip ls (listaAdicao ls)

-- Adiciona n a todos os numeros de uma lista
listaAdicionada n ls = [fst x + snd x | x <- listaZip (adicionaALista n (letrasParaNumeros ls))]

-- Devolve a lista pronta para ser transformada em letras na cifrar Obelix
listaPronta n ls = rotacao (listaAdicionada n ls)

-- Devolve uma lista diminiuda n numeros mais a posicao de cada algarismo na lista
listaDiminuida n ls = [fst x - snd x | x <- listaZip (adicionaALista (-n) (letrasParaNumeros ls))]

-- Devolve a lista pronta para ser transformada em letras na decifrar Obelix
listaPronta2 n ls = rotacao (listaDiminuida n ls)

-- CIFRAR E DECIFRAR OBELIX
cifrarObelix n st = numerosParaLetras (listaPronta n st)
decifrarObelix n st = numerosParaLetras (listaPronta2 n st)

-- Transforma um numero em uma String
numeroEmString x = show x

-- Verifica se um dado numero eh Palindromos
ehPalindromo x = numeroEmString x == reverse (numeroEmString x)

-- Devolve uma lista de numeros Palindromos ate um certo numero
listaDePalindromos x = if x < 0 then listaDePalindromosNeg x else [x | x <- [0 .. x], ehPalindromo x]

-- Devolve uma lista de numeros Palindromos negativos ate um certo numero
listaDePalindromosNeg x = [x | x <- [0, -1 .. x], ehPalindromo (-x)]

-- Devolve o ultimo numero Palindromo perto de um dado numero
numPa x = listaDePalindromos x !! (length (listaDePalindromos x) - 1)

-- TRES PALINDROMOS
tresPalindromos x = (numPa x, numPa (x - numPa x), numPa ((x - numPa x) - numPa (x - numPa x)))
