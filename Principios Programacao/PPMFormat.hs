{-
- Projeto
- Guilherme Teixeira - 49021
-
-}
module PPMFormat
(
PPM (PPM),
ppmWidth,
ppmHeight,
ppmLength,
ppmMaxSize,
ppmPixels,
cnvrt2PPMFile,
formatPPM,
dropLastColumn,
dropLastLine,
Pixel (Pixel),
r,
g,
b,
avr,
inRange,
) where

import Data.List
import Test.QuickCheck
import System.Random
-- Tipo PPM e Pixel
data PPM = PPM Int Int Int [Pixel] Int deriving Show
data Pixel = Pixel Int Int Int

-- Width do PPM
ppmWidth :: PPM -> Int
ppmWidth (PPM wdth _ _ _ _) = wdth

-- Height do PPM
ppmHeight :: PPM -> Int
ppmHeight (PPM _ hght _ _ _) = hght

-- Numero de pixeis do PPM
ppmLength :: PPM -> Int
ppmLength (PPM _ _ _ _ lngt) = lngt

-- Maximo taminho do PPM
ppmMaxSize :: PPM -> Int
ppmMaxSize (PPM _ _ maxSize _ _) = maxSize

-- Todos Pixeis do PPM
ppmPixels :: PPM -> [Pixel]
ppmPixels (PPM _ _ _ pxls _) = pxls

-- Converte um PPM para String
cnvrt2PPMFile :: PPM -> String
cnvrt2PPMFile ppm = "P3\n" ++ show (ppmWidth ppm) ++ " " ++ show (ppmHeight ppm) ++ "\n" ++ show (ppmMaxSize ppm) ++ "\n" ++ parsePixels (ppmWidth ppm) (ppmPixels ppm)

-- Dada uma width e uma lista de Pixeis converte para String
parsePixels :: Int -> [Pixel] -> String
parsePixels width pxs
    |pxs == [] = []
    |otherwise = concat (map show (take width pxs)) ++ "\n" ++ parsePixels width (drop width pxs)

-- Dada uma String converte-a para PPM
formatPPM :: String -> PPM
formatPPM ppmFile = cnvrt2Pixel(drop 1 (words (intercalate " " (removeComents (lines ppmFile)))))

-- Dada uma lista de String converte-a para PPM
cnvrt2Pixel :: [String] -> PPM
cnvrt2Pixel vec = PPM (read (vec !! 0) :: Int) (read (vec !! 1) :: Int) (read (vec !! 2) :: Int) (makePixel (drop 3 vec)) (div (length (drop 3 vec)) 3)

-- Dada uma lista de String converte-a para lista de Pixeis
makePixel :: [String] -> [Pixel]
makePixel vec
    |vec == [] = []
    |otherwise = [Pixel (read (vec !! 0) :: Int) (read (vec !! 1) :: Int) (read (vec !! 2) :: Int)] ++ makePixel (drop 3 vec)

-- Dada uma lista de String retira os comentarios dela
removeComents :: [String] -> [String]
removeComents ppmFile
    |ppmFile == [] = []
    |otherwise = if elem '#' (ppmFile !! 0)
                 then [trimLine (ppmFile !! 0)] ++ removeComents (drop 1 ppmFile)
                 else [ppmFile !! 0] ++ removeComents (drop 1 ppmFile)

-- Dada uma String tira os comentarios
trimLine :: String -> String
trimLine str
    |str !! 0 == '#' = " "
    |otherwise = [str !! 0] ++ trimLine (drop 1 str)

-- Retira a ultima linha de Pixeis do PPM
dropLastLine :: PPM -> PPM
dropLastLine (PPM wdt hgt mxV pxs len) = PPM wdt (hgt - 1) mxV (take (len - wdt) pxs) len

-- Retira a ultima coluna de Pixeis do PPM
dropLastColumn :: PPM -> PPM
dropLastColumn (PPM wdt hgt mxV pxs len) = PPM (wdt - 1) hgt mxV (removeCol pxs wdt) len

-- Retira a ultima coluna de Pixeis de uma lista de Pixeis
removeCol :: [Pixel] -> Int -> [Pixel]
removeCol pxs wdt
    |pxs == [] = []
    |otherwise = take (wdt - 1) pxs ++ removeCol (drop wdt pxs) wdt

-- Cor vermelha de um Pixel
r :: Pixel -> Int
r (Pixel red _ _) = red

-- Cor verde de um Pixel
g :: Pixel -> Int
g (Pixel _ green _) = green

-- Cor azul de um Pixel
b :: Pixel -> Int
b (Pixel _ _ blue) = blue

-- Media entre dois Pixeis
avr :: Pixel -> Pixel -> Pixel
avr px1 px2 = Pixel (div (r px1 + r px2) 2) (div (g px1 + g px2) 2) (div (b px1 + b px2) 2)

-- Se um Pixel esta a baixo de um valor maxio
inRange :: Pixel -> Int -> Bool
inRange px maxVal = (r px) <= maxVal && (g px) <= maxVal && (b px) <= maxVal

-- Instacia Eq do Pixel
instance Eq Pixel where
    pxa == pxb = r pxa == r pxb && g pxa == g pxb && b pxa == b pxb

-- Instacia Show do Pixel
instance Show Pixel where
    show (Pixel ah be ce) = show ah ++ " " ++ show be ++ " " ++ show ce ++ " "

-- Instacia Eq do PPM
instance Eq PPM where
    ppma == ppmb = (cnvrt2PPMFile ppma) == (cnvrt2PPMFile ppmb)

-- Instacia Arbitrary do Pixel
instance Arbitrary Pixel where
    arbitrary = do
        ar <- choose (0, 255)
        ge <- choose (0, 255)
        be <- choose (0, 255)
        return (Pixel ar ge be)

-- Instacia Arbitrary do PPM
instance Arbitrary PPM where
    arbitrary = do
        maxValue <- elements [1, 3, 7, 15, 31, 63, 127, 255]
        wdth <- choose (1, 64)
        hgth <- choose (1, 64)
        lgth <- elements [wdth * hgth]
        pxs <- pxList lgth maxValue
        return (PPM wdth hgth maxValue pxs lgth)

-- Lista de Pixel aleatoria
pxList :: Int -> Int -> Gen([Pixel])
pxList n maxValue = if n == 0 then return [] else do
    ar <- choose (0,maxValue)
    ge <- choose (0,maxValue)
    be <- choose (0,maxValue)
    rs <- pxList (n-1) maxValue
    return ((Pixel ar ge be) : rs)
