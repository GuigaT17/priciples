{-
Trabalho 4
Guilherme Teixeira - 49021
-}

module Expressao (
Expr,
constante,
(|+|),
(|*|),
avalia,
Eq,
Show
) where

-- O tipo Expr
data Expr = Constante Int | Oper Expr String Expr

-- Tipo constante do tipo Expr
constante x = Constante x

-- Funcao de + no tipo Expr Oper
(|+|) :: Expr -> Expr -> Expr
(|+|) x y = Oper x " + " y

-- Funcao de * no tipo Expr Oper
(|*|) :: Expr -> Expr -> Expr
(|*|) x y = Oper x " * " y

-- Avalia todos os tipos de Expr
avalia :: Expr -> Int
avalia (Constante x) = x
avalia (Oper x " * " y) = (avalia x) * (avalia y)
avalia (Oper x " + " y) = (avalia x) + (avalia y)

-- Instancia de Eq, avalia se duas expressoes sao iguais
instance Eq Expr where
  x == y = if (avalia x) == (avalia y) then True else False

-- Transforma a expressao em sua representacao String
toStr :: Expr -> String
toStr (Constante x) = show x
toStr (Oper x str y) = "(" ++ (toStr x) ++ str ++ (toStr y) ++ ")"

-- Instancia show que mostra a expressao em forma de String
instance Show Expr where
  show x = toStr x
