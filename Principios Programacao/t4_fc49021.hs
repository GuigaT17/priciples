{-
Trabalho 4
Guilherme Teixeira - 49021
-}
import Expressao
expr1 :: Expr
expr1 = constante 3 |+| (constante 10 |*| constante 3)
expr2 :: Expr
expr2 = constante 3 |*| (constante 10 |+| constante 1)
main :: IO ()
main = do
putStrLn $ show expr1 ++ " = " ++ show (avalia expr1)
putStrLn $ show expr2 ++ " = " ++ show (avalia expr2)
putStrLn $ show $ expr1 == expr2
putStrLn $ show $ expr1 == constante 0
