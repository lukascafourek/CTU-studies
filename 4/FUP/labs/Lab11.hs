import GHC.Float (geDouble)
toUpper :: Char -> Char
toUpper c = case lookup c $ zip ['a'..'z'] ['A'..'Z'] of
    Nothing -> c
    Just c' -> c'

toCamelCase :: String -> String
toCamelCase = concat . map toUpperHead . words where
    toUpperHead "" = ""
    toUpperHead (x:xs) = toUpper x:xs

toCamelCaseF :: Functor f => f String -> f String
toCamelCaseF = fmap toCamelCase

data DFA a = Automaton (a->Char->a) a (a->Bool)

evalDFA :: DFA a -> String -> Bool
evalDFA (Automaton dlt s inF) w =
    inF (foldl dlt s w)
--  inF (deltaStar s w)
--  where deltaStar q [] = q
--       deltaStar q (a:ws) = deltaStar (dlt q a) ws

data State = Before | Digit | Dot | First | Second | Fail

isNum :: Char -> Bool
isNum c = c `elem` ['0'..'9']

final :: State -> Bool
final Second = True
final _ = False

delta :: State -> Char -> State
delta Before c | isNum c = Digit
               | otherwise = Fail
delta Digit c | isNum c = Digit
              | c == '.' = Dot
              | otherwise = Fail
delta Dot c | isNum c = First
            | otherwise = Fail
delta First c | isNum c = Second
              | otherwise = Fail
delta Second _ = Fail
delta Fail _ = Fail

automaton :: DFA State
automaton = Automaton delta Before final

parseNum :: String -> Maybe Float
parseNum w = if evalDFA automaton w then Just (read w)
             else Nothing

parseNumF :: Functor f => f String -> f (Maybe Float)
parseNumF = fmap parseNum

parseIO :: IO ()
parseIO = putStrLn "Enter number:"
          >> parseNumF getLine
          >>= \x -> case x of
                      Nothing -> parseIO
                      Just _ -> putStrLn "Ok"

-- parseIO :: IO ()
-- parseIO = do putStrLn "Enter number:"
--              x <- parseNumF getLine
--              case x of
--                Nothing -> parseIO
--                Just _ -> putStrLn "Ok"

data Expr a = Atom a
            | Neg (Expr a)
            | And (Expr a) (Expr a) 
            | Or (Expr a) (Expr a) 
                deriving (Eq, Show)

expr :: Expr Bool
expr = Or (And (Atom True) (Neg (Atom False))) (Atom False) 

fle :: Expr String
fle = And (Or (Neg (Atom "x")) (Atom "x")) (Atom "y")

eval :: Expr Bool -> Bool
eval (Atom c) = c
eval (Neg e) = not (eval e)
eval (And e1 e2) = eval e1 && eval e2
eval (Or e1 e2) = eval e1 || eval e2
 
getAtoms :: Expr a -> [a]
getAtoms (Atom c) = [c]
getAtoms (Neg e) = getAtoms e
getAtoms (And e1 e2) = getAtoms e1 ++ getAtoms e2
getAtoms (Or e1 e2) = getAtoms e1 ++ getAtoms e2

instance Functor Expr where
    fmap f (Atom c) = Atom (f c)
    fmap f (Neg e) = Neg (fmap f e)
    fmap f (And e1 e2) = And (fmap f e1) (fmap f e2)
    fmap f (Or e1 e2) = Or (fmap f e1) (fmap f e2)

subst :: Functor f => [String] -> f String -> f Bool
subst xs = fmap (`elem` xs)

subseqs :: [a] -> [[a]]
subseqs [] = [[]]
subseqs (x:xs) = subseqs xs ++ [x:ys | ys <- subseqs xs]

check :: ([Bool] -> Bool) -> Expr String -> Bool
check g e = g [ eval $ subst vs e | vs <- vss] 
    where vss = subseqs $ getAtoms e
 
isTaut, isSat :: Expr String -> Bool
isTaut = check and
isSat = check or

main = do
    print(toCamelCaseF [" no air ", " get back"])
    print(toCamelCaseF (Just " no air "))
    print(parseNumF ["234", "123.12", ".5", "0.50"])
