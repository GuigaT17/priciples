{-
- Projeto
- Guilherme Teixeira - 49021
-
-}
import PPMFormat
import System.Environment
import System.IO
import Data.List
import Test.QuickCheck

dispatch :: [(String, PPM -> PPM)]
dispatch =  [("-gs", gs)
            ,("-hh", hh)
            ,("-hw", hw)
            ,("-fv", fv)
            ,("-fh", fh)
            ,("-rc", rc)
            ,("-gc", gc)
            ,("-bc", bc)
            ]

main = do
    args <- getArgs
    if (args !! 0) == "-t"
    then doTests
    else do
        contents <- readFile (args !! 0)
        if length args > 2
        then writeFile (args !! 1) (cnvrt2PPMFile (processIt (drop 2 args) (formatPPM contents)))
        else writeFile (args !! 1) contents

-- -----------------------------------------------------------------------------
-- QuickTest que a inversao dupla eh igual a nenhuma inversao
prop_ppm_doubleInversion :: PPM -> Bool
prop_ppm_doubleInversion ppm = (ppm == (fh (fh (ppm))) && ppm == (fv (fv (ppm))))
-- QuickTest do tamanho com a quantidade de Pixeis
prop_ppm_size :: PPM -> Bool
prop_ppm_size ppm = (ppmLength ppm) == (ppmWidth ppm * ppmHeight ppm)
-- QuickTest De que o maximo valor eh maior que todos os Pixeis§
prop_ppm_maxValue :: PPM -> Bool
prop_ppm_maxValue ppm = let maxSize = ppmMaxSize ppm; pxs = ppmPixels ppm
                          in [] == (filter (==False) (map (`inRange` maxSize) pxs))

doTests = do
    quickCheck prop_ppm_size
    quickCheck prop_ppm_doubleInversion
    quickCheck prop_ppm_maxValue

-- -----------------------------------------------------------------------------

processIt :: [String] -> PPM -> PPM
processIt args ppm
    |args == [] = ppm
    |otherwise = let (Just action) = lookup (args !! 0) dispatch
        in processIt (drop 1 args) (action ppm)

-- -------------------------------------------------------------------------------------
-- PPM que deixa apenas a cor azul
bc :: PPM -> PPM
bc ppm = PPM (ppmWidth ppm) (ppmHeight ppm) (ppmMaxSize ppm) (bcAux (ppmPixels ppm)) (ppmLength ppm)

-- Lista de Pixeis que deixa apenas a cor azul
bcAux :: [Pixel] -> [Pixel]
bcAux vec
    |vec == [] = []
    |otherwise = [Pixel 0 0 $ b $ vec !! 0] ++ (bcAux $ drop 1 vec)

-- PPM que deixa apenas a cor verde
gc :: PPM -> PPM
gc ppm = PPM (ppmWidth ppm) (ppmHeight ppm) (ppmMaxSize ppm) (gcAux (ppmPixels ppm)) (ppmLength ppm)

-- Lista de Pixeis que deixa apenas a cor verde
gcAux :: [Pixel] -> [Pixel]
gcAux vec
    |vec == [] = []
    |otherwise = [Pixel 0 (g $ vec !! 0) 0] ++ (gcAux $ drop 1 vec)

-- PPM que deixa apenas a cor vermelha
rc :: PPM -> PPM
rc ppm = PPM (ppmWidth ppm) (ppmHeight ppm) (ppmMaxSize ppm) (rcAux (ppmPixels ppm)) (ppmLength ppm)

-- Lista de Pixeis que deixa apenas a cor vermelha
rcAux :: [Pixel] -> [Pixel]
rcAux vec
    |vec == [] = []
    |otherwise = [Pixel (r $ vec !! 0) 0 0] ++ (rcAux $ drop 1 vec)

-- Inverte horizontalmente um PPM
fh :: PPM -> PPM
fh ppm = let width = ppmWidth ppm
            in PPM (width) (ppmHeight ppm) (ppmMaxSize ppm) (fhAux width (ppmPixels ppm)) (ppmLength ppm)

-- Inverte horizontalmente uma lista de Pixeis
fhAux :: Int -> [Pixel] -> [Pixel]
fhAux width vec
    |vec == [] = []
    |otherwise = (reverse $ take width vec) ++ (fhAux width $ drop width vec)

-- Inverte verticalmente um PPM
fv :: PPM -> PPM
fv ppm = let width = ppmWidth ppm
            in PPM (width) (ppmHeight ppm) (ppmMaxSize ppm) (fvAux width (ppmPixels ppm)) (ppmLength ppm)

-- Inverte verticalmente uma lista de Pixeis
fvAux :: Int -> [Pixel] -> [Pixel]
fvAux width vec = fhAux width $ reverse vec

-- Faz a metade da largura de um PPM
hw :: PPM -> PPM
hw (PPM wdt hgt mxV pxs len) = if rem wdt 2 == 0
    then PPM (div wdt 2) hgt mxV (hwAux pxs) len
    else let (PPM wdt2 hgt2 mxV2 pxs2 len2) = dropLastColumn (PPM wdt hgt mxV pxs len)
                                        in PPM (div wdt2 2) hgt2 mxV2 (hwAux pxs2) len2

-- Faz metade da largura de uma lista de pixeis
hwAux :: [Pixel] -> [Pixel]
hwAux vec
    |vec == [] = []
    |otherwise = [avr (vec !! 0) (vec !! 1)] ++ (hwAux $ drop 2 vec)

-- Faz a metade da altura de um PPM
hh :: PPM -> PPM
hh (PPM wdt hgt mxV pxs len) = if rem hgt 2 == 0
    then PPM wdt (div hgt 2) mxV (hhAux wdt pxs) len
    else let (PPM wdt2 hgt2 mxV2 pxs2 len2) = dropLastLine (PPM wdt hgt mxV pxs len)
                                        in PPM wdt2 (div hgt2 2) mxV2 (hhAux wdt2 pxs2) len2

-- Faz metade da altura de uma lista de pixeis
hhAux :: Int -> [Pixel] -> [Pixel]
hhAux width vec
    |vec == [] = []
    |otherwise = lineOp (take width vec) (drop width (take (2*width) vec)) ++ hhAux width (drop (width*2) vec)

lineOp :: [Pixel] -> [Pixel] -> [Pixel]
lineOp line1 line2
    |line1 == [] = []
    |otherwise = [avr (line1 !! 0) (line2 !! 0)] ++ lineOp (drop 1 line1) (drop 1 line2)

-- Um PPM fica na gray scale
gs :: PPM -> PPM
gs ppm = PPM (ppmWidth ppm) (ppmHeight ppm) (ppmMaxSize ppm) (gsAux (ppmPixels ppm)) (ppmLength ppm)

-- Uma lista de Pixeis fica na gray scale
gsAux :: [Pixel] -> [Pixel]
gsAux vec
    |vec == [] = []
    |otherwise = [convertGS (vec !! 0)] ++ gsAux (drop 1 vec)

convertGS :: Pixel -> Pixel
convertGS px = let val = round (0.2126 * (fromIntegral (r px)) + 0.7152 * (fromIntegral (g px)) + 0.0722 * (fromIntegral (b px)))
                    in Pixel val val val
