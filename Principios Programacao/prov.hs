import Data.List
import Data.Maybe
import Data.Char
import Test.QuickCheck
import System.Random
import Control.Monad
--
encontra :: (a -> Bool) -> [a] -> Maybe a
encontra f [] = Nothing
encontra f (x:xs) = if (f x) then Just x else (encontra f xs)

fatias :: Int -> [a] -> [[a]]
fatias x [] = []
fatias y xs  | y <= length xs = take y xs : fatias y (drop y xs)
             | otherwise      = [xs]

keepUntil :: [Char] -> String -> [Char]
keepUntil [] y = []
keepUntil (x:xs) y = if (elem x y) then [] else [x] ++ (keepUntil xs y)

separa :: [Char] -> [Char] -> [[Char]]
separa [] [] = []
separa ys [] = []
separa [] xs = [xs]
separa ys xs = [gh] ++ separa ys (drop quant xs)
                where gh = keepUntil xs ys
                      quant = length gh
--
data Doc = Vazio | Texto String | NovaLinha | Concat Doc Doc
instance Show Doc where
    show (Vazio) = ""
    show (Texto x) = x
    show (NovaLinha) = "\n"
    show (Concat x y) = (show x) ++ (show y)

instance Eq Doc where
  x == y = show x == show y
--
instance Arbitrary Doc where
  arbitrary = oneof [ return Vazio,
                      return NovaLinha,
                      liftM Texto arbitrary,
                      liftM2 Concat arbitrary arbitrary]

prop_concat_vazio :: Doc -> Bool
prop_concat_vazio x = Concat x Vazio == x && Concat Vazio x == x

prop_concat_associo :: Doc -> Doc -> Doc -> Bool
prop_concat_associo x y z = Concat x (Concat y z) == Concat (Concat x y) z
--------------------------------------------
--PROVA 2 2018
linhaBifid :: Char -> Int
linhaBifid x
            | elem x "BGWKZ" = 1
            | elem x "QPNDS" = 2
            | elem x "IJOAXE" = 3
            | elem x "FCLUM" = 4
            | otherwise = 5

colunaBifid :: Char -> Int
colunaBifid x
              | elem x "BQIJFT" = 1
              | elem x "GPOCH" = 2
              | elem x "WNALY" = 3
              | elem x "KDXUV" = 4
              | otherwise = 5

-- 1A
linhasBd :: [Char] -> [Int]
linhasBd [] = []
linhasBd (x:xs) = linhaBifid x : linhasBd xs

colunasBd :: [Char] -> [Int]
colunasBd [] = []
colunasBd (x:xs) = colunaBifid x : colunasBd xs

coordenadas :: String -> ([Int], [Int])
coordenadas [] = ([], [])
coordenadas x = (linhasBd x, colunasBd x)

-- 1B
letra :: Int -> Int -> Char
letra l c
          | l == 1 = " BGWKZ" !! c
          | l == 2 = " QPNDS" !! c
          | l == 3 = " IOAXE" !! c
          | l == 4 = " FCLUM" !! c
          | otherwise = " THYVR" !! c

palavra :: [Int] -> String
palavra [] = []
palavra [x] = []
palavra x = letra (x!!0) (x!!1) : (palavra $ drop 2 x)

-- 1C
bifid :: String -> String
bifid x = palavra (linhasBd x ++ colunasBd x)

-- 2
data Exp = Variavel Char | Inteiro Int | Mais Exp Exp | Vezes Exp Exp

-- 2A
comprimento :: Exp -> Int
comprimento (Variavel x) = 0
comprimento (Inteiro x) = 0
comprimento (Mais x y) = 1 + comprimento x + comprimento y
comprimento (Vezes x y) = 1 + comprimento x + comprimento y

-- 2B
avalia :: [(Char, Int)] -> Exp -> Int
avalia xs (Variavel x)
                      | n == Nothing = 0
                      | otherwise = fromJust n
                      where n = lookup x xs
avalia xs (Inteiro x) = x
avalia xs (Mais x y) = (avalia xs x) + (avalia xs y)
avalia xs (Vezes x y) = (avalia xs x) * (avalia xs y)

-- 2C
instance Show Exp where
    show (Variavel x) = [x]
    show (Inteiro x) = show x
    show (Mais x y) = (show x) ++ "+" ++ (show y)
    show (Vezes (Mais z w) d) = "(" ++ (show (Mais z w)) ++ ")" ++ show d
    show (Vezes e (Mais z w)) = show e ++ "(" ++ (show (Mais z w)) ++ ")"
    show (Vezes (Mais x y) (Mais z w)) = "(" ++ (show (Mais x y)) ++ ")" ++ "(" ++ (show (Mais z w)) ++ ")"
    show (Vezes x y) = show x ++ show y

-- 3A
instance Arbitrary Exp where
  arbitrary = oneof [ liftM Variavel arbitrary,
                      liftM Inteiro arbitrary,
                      liftM2 Mais arbitrary arbitrary,
                      liftM2 Vezes arbitrary arbitrary ]

-- 3B
prop_comp_nneg :: Exp -> Bool
prop_comp_nneg x = comprimento x >= 0
--

digito :: Int -> [Int]
digito 0 = []
digito x = (x `mod` 10) : (digito (div x 10))

func :: (Eq a, Num a) => (a -> Bool) -> a -> a
func f x
        | f x = 1
        | otherwise = 0

especial :: Int -> Bool
especial x = (sum $ map (^3) (digito x)) == x

pedirEspeciais :: IO()
pedirEspeciais = do
                  n <- getLine
                  if n == "0" then return ()
                  else do
                    if especial $ read n
                      then do
                        print "SIM"
                        pedirEspeciais
                      else do
                        print "NAO"
                        pedirEspeciais

countExist :: (a -> b -> Bool) -> a -> [b] -> Int
countExist f a [] = 0
countExist f a (x:xs) = if f a x then 1 + countExist f a xs else countExist f a xs

writeString :: String -> IO()
writeString [] = return ()
writeString (x:xs) = do
                    putChar x
                    writeString xs
---------------------------
juntaLinha :: [String] -> String
juntaLinha [] = ""
juntaLinha [x] = x
juntaLinha (x:xs) = x ++ " " ++ juntaLinha xs

apagaDuplicados :: Eq a => [a] -> [a]
apagaDuplicados [] = []
apagaDuplicados xs = foldl(\acc x -> if (length acc) > 0 && (acc !! ((length acc) - 1)) == x then acc
  else acc ++ [x])[] xs

--
data Arvore = Folha | No Int (Arvore) (Arvore)
arvorePLista :: Arvore -> [Int]
arvorePLista (Folha) = []
arvorePLista (No x e d) = x : ((arvorePLista e) ++ (arvorePLista d))

minimo :: Arvore -> Maybe Int
minimo (Folha) = Nothing
minimo (No x Folha _) = Just x
minimo (No x e _) = minimo e

instance Eq Arvore where
  x == y = (sort $ arvorePLista x) == (sort $ arvorePLista y)

{-data Exp = Val Int | Soma Exp Exp | Produto Exp Exp

estatistica :: Exp -> (Int, Int, Int)
estatistica (Val x) = (0, 0, 1)
estatistica (Soma e d) = (0, 1, 0) + (estatistica e) + (estatistica d)
estatistica (Produto e d) = (1, 0, 0) + (estatistica e) + (estatistica d)
---------------------------
zip31 :: [a] -> [b] -> [c] -> [(a, b, c)]
zip31 [] x y = []
zip31 x [] y = []
zip31 x y [] = []
--zip31 (x:xs) (y:ys) (z:zs) = (x, y, z) : zip31 xs ys zs
-----}

data Html = Dic [Html] | Text String | Negrito String
instance Arbitrary Html where
  arbitrary = oneof [liftM Dic arbitrary,
                     liftM Text arbitrary,
                     liftM Negrito arbitrary]

toStringH :: [Html] -> String
toStringH (x:xs) = show x ++ (toStringH xs)

instance Show Html where
  show (Text x) = show x
  show (Negrito x) = "<b>" ++ show x ++ "<0b>"
  show (Dic xs) = "<dic>" ++ show xs ++ "<0dic>"

profmax :: [Html] -> Int
profmax [] = 0
profmax [x] = 1
profmax (x:xs) = max (profundidade x) (profmax xs)

profundidade :: Html -> Int
profundidade (Text x) = 1
profundidade (Negrito x) = 1
profundidade (Dic []) = 1
profundidade (Dic xs) = 1 + profmax xs

--

jnta :: [Char] -> [String] -> String
jnta [] (x:xs) = x ++ jnta [] xs
jnta _ [] = []
jnta y (x:xs) = (x ++ y) ++ jnta y xs

--

entremul :: Int -> Int -> Int -> Int
entremul a b c
                | a >= b = 0
                | a `mod` c == 0 = 1 + entremul (a + 1) b c
                | otherwise = entremul (a + 1) b c

pegaUm :: [[a]] -> [a]
pegaUm [] = []
pegaUm [[]] = []
pegaUm (x:xs) = (take 1 x) ++ (pegaUm xs)

tiraUm :: [[a]] -> [[a]]
tiraUm [] = []
tiraUm [[]] = []
tiraUm ((x:xs):ys) = [xs] ++ (tiraUm ys)

provaHJ :: [[a]] -> [[a]]
provaHJ [] = []
provaHJ xs = [pegaUm xs] ++ (provaHJ $ tiraUm xs)
--
