module Hw3 where

type Symbol = String
data Expr = Var Symbol
          | App Expr Expr
          | Lambda Symbol Expr deriving Eq

instance Show Expr where
  show (Var x) = x
  show (App ex1 ex2) = "(" ++ show ex1 ++ " " ++ show ex2 ++ ")"
  show (Lambda x ex) = "(\\" ++ x ++ "." ++ show ex ++ ")"

newVar :: Int -> Symbol
newVar n = "a" ++ show n

substitution :: Symbol -> Expr -> Expr -> Int -> Expr
substitution x ex1 (Var y) n = if x == y then ex1 else Var y
substitution x ex1 (App ex2 ex3) n = App (substitution x ex1 ex2 n) (substitution x ex1 ex3 n)
substitution x ex1 (Lambda y ex2) n
  | x == y = Lambda y ex2
  | y `elem` free ex1 = let z = newVar n in Lambda z (substitution x ex1 (rename y ex2 z) (n + 1))
  | otherwise = Lambda y (substitution x ex1 ex2 n)

rename :: Symbol -> Expr -> Symbol -> Expr
rename old (Var y) new = if old == y then Var new else Var y
rename old (App ex1 ex2) new = App (rename old ex1 new) (rename old ex2 new)
rename old (Lambda y ex) new = Lambda y (if old == y then ex else rename old ex new)
                                
free :: Expr -> [Symbol]
free (Var x) = [x]
free (App ex1 ex2) = free ex1 ++ free ex2
free (Lambda x ex) = filter (/= x) (free ex)

reduction :: Expr -> Int -> Maybe Expr
reduction (App (Lambda x ex) ex2) n = Just (substitution x ex2 ex n)
reduction (App ex1 ex2) n = case reduction ex1 n of
                            Just ex1_2 -> reduction (App ex1_2 ex2) n
                            Nothing -> reduction ex2 n >>= \ex2_2 -> return (App ex1 ex2_2)
reduction (Lambda x ex) n = Lambda x <$> reduction ex n
reduction _ _ = Nothing

eval :: Expr -> Expr
eval ex = eval2 ex 0
  where eval2 ex n = case reduction ex n of
                      Just ex2 -> eval2 ex2 n
                      Nothing -> ex
