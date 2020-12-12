{-
Trabalho 3
Guilherme Teixeira - 49021
-}
import Data.Char

-- Dada uma String para formacao de pares com nota e sua oitava
fromString :: [Char] -> [(Char, Int)]
fromString [] = []
fromString (x:y:xs) = (x,read [y] + 0) : fromString xs

-- Dada uma nota retorna o indice dela
posi :: Char -> Int  -> [Char] -> Int
posi x acc ls = if x == head ls then acc else posi x (acc + 1) (drop 1 ls)

-- Dada um par com uma nota e sua oitava retorna o seu midi
midi :: (Char, Int) -> Int
midi (x, y) = y * 12 + (posi x 0 "CcDdEFfGgAaB") + 12

-- Dada um par com uma nota e sua oitava retorna a diferenca entre ela e a (A,4)
dif :: (Char, Int) -> Double
dif x = fromIntegral (midi x - midi ('A', 4))

-- Dada um par com uma nota e sua oitava retorna sua frequencia
frequencia :: (Char, Int) -> Double
frequencia x = 440.0 * (2.0 ** ((dif x) / 12.0))

-- Dada uma lista de pares com nota e sua oitava devolve uma lista com sua frequencia
fromNotes :: [(Char, Int)] -> [Double]
fromNotes xs = map frequencia xs

-- Dada uma nota e sua oitava diz se sua frequencia eh menor que um dado numero
ehMenor :: Double -> (Char, Int) -> Bool
ehMenor x nota = (frequencia nota) < x

-- Dada uma lista de notas devolve apenas as notas com frequencia abaixo de uma certo numero
notesBelow :: Double -> [(Char, Int)] -> [(Char, Int)]
notesBelow x y = filter (ehMenor x) y

-- Dada uma lista de notas devolve a media das frequencias
averageFrequency :: [(Char, Int)] -> Double
averageFrequency x = (foldl (\acc x -> acc + (frequencia x)) 0 x) / (fromIntegral (length x))
