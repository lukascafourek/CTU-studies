import Data.Char
import Control.Applicative

data Block = W | F | S deriving (Eq,Show)

data Maze = M [[Block]]

maze :: Maze   -- a testing maze
maze = M [[W,W,W,W,W],
          [W,F,W,F,W],
          [W,F,W,W,W],
          [W,F,F,F,W],
          [W,W,W,W,W]]

instance Show Maze where
    show (M []) = ""
    show (M (r:rs)) = map dispBlock r ++ "\n" ++ show (M rs)
       where dispBlock W = '#'
             dispBlock F = ' '
             dispBlock S = '*'

type Pos = (Int, Int)
type Path = [Pos]
type Task = (Pos,Pos,Maze)

safePut :: Int -> a -> [a] -> Maybe [a]
safePut n x xs | 0 <= n && n < length xs = Just $ take n xs ++ [x] ++ drop (n+1) xs
               | otherwise = Nothing

safeGet :: Int -> [a] -> Maybe a
safeGet n xs | n `elem` [0..length xs-1] = Just $ xs !! n
             | otherwise = Nothing

getBlock :: Pos -> Maze -> Maybe Block
getBlock (x,y) (M xss) = do row <- safeGet y xss
                            block <- safeGet x row
                            return block

setBlock :: Block -> Pos -> Maze -> Maybe Maze
setBlock b (x,y) (M xss) = do row <- safeGet y xss
                              row' <- safePut x b row
                              xss' <- safePut y row' xss
                              return (M xss')

setPath :: Maze -> Path -> Maybe Maze
setPath m [] = Just m
setPath m (p:ps) = do m' <- setBlock S p m
                      m'' <- setPath m' ps
                      return m''

drawSol :: Maze -> Path -> Maze
drawSol m ps = case setPath m ps of
                 Nothing -> m
                 Just m' -> m'

neighbs :: Pos -> [Pos]
neighbs (x,y) = [(x-1,y), (x+1,y), (x,y-1), (x,y+1),
                 (x-1,y-1), (x-1,y+1), (x+1,y-1), (x+1,y+1)]

nextPos :: Pos -> Maze -> [Pos]
nextPos p m = case getBlock p m of                                          -- is the input position admissible?
                Just F -> [ p' | p' <- neighbs p, getBlock p' m == Just F]  -- if yes, take all possibilities and filter admissible positions
                _ -> []

extend :: Path -> Maze -> [Path]
extend [] _ = []
extend path@(p:_) m = map (:path) $ nextPos p m

solve :: Task -> Maybe Path
solve (p,q,m) = bfs [] [[p]] q m

bfs :: [Pos] -> [Path] -> Pos -> Maze -> Maybe Path
bfs _ [] _ _ = Nothing
bfs visited (path@(p:_):paths) q m                       -- consider the first path in the queue and its head p
    | p == q = Just $ reverse path                       -- is path a solution? If yes, return the reversed solution
    | p `elem` visited = bfs visited paths q m           -- does path end in an already visited position? If yes, disregard it
    | otherwise = bfs (p:visited) (paths ++ extend path m) q m  -- add p to visited positions and extend path by all possible positions

newtype Parser a = P { parse :: String -> Maybe (a, String) }

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p = P (\inp -> case parse p inp of
                            Nothing -> Nothing
                            Just (v,out) -> Just (f v, out))

instance Applicative Parser where
    -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    pg <*> px = P (\inp -> case parse pg inp of
                             Nothing -> Nothing
                             Just (g,out) -> parse (fmap g px) out)
    pure v = P (\inp -> Just (v,inp))

instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = P (\inp -> case parse p inp of
                           Nothing -> Nothing
                           Just (v,out) -> parse (f v) out)

instance Alternative Parser where
    -- empty :: Parser a
    empty = P (\_ -> Nothing)
    -- (<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = P (\inp -> case parse p inp of
                           Nothing -> parse q inp
                           Just (v,out) -> Just (v,out))

item :: Parser Char
item = P (\inp -> case inp of
                    "" -> Nothing
                    (x:xs) -> Just (x,xs))

sat :: (Char -> Bool) -> Parser Char
sat pr = do x <- item
            if pr x then return x
            else empty

digit :: Parser Char
digit = sat isDigit

char :: Char -> Parser Char
char c = sat (== c)

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

-- string :: String -> Parser String
-- string [] = return []
-- string (x:xs) = char x *> string xs *> pure (x:xs)

space :: Parser ()
space = do many (sat isSpace)
           return ()

token :: Parser a -> Parser a
token p = do space
             x <- p
             space
             return x

-- space :: Parser ()
-- space = do many (sat isSpace) *> pure ()

-- token :: Parser a -> Parser a
-- token p = space *> p <* space

pos :: Parser Pos
pos = do char '('                -- it has to start with '('
         space                   -- possibly followed by spaces
         x <- some digit         -- then parses a nonempty sequence of digits
         token (char ',')        -- then comma possible surrounded by spaces
         y <- some digit         -- then a second non-empty sequence of digits
         space                   -- possibly spaces
         char ')'                -- then the closing ')'
         return (read x, read y) -- the position is returned, sequences of digits are converted by read

def :: String -> Parser Pos
def str = do string str
             token (char '=')
             p <- pos
             char '\n'
             return p

wall :: Parser Block
wall = do char '#'
          return W

free :: Parser Block
free = do char ' '
          return F

row :: Parser [Block]
row = do bs <- many (wall <|> free)
         char '\n'
         return bs

-- wall :: Parser Block
-- wall = char '#' *> pure W

-- row :: Parser [Block]
-- row = many (wall <|> free) <* char '\n'

mapP :: Parser Maze
mapP = do rs <- many row
          return (M rs)

file :: Parser Task
file = do p <- def "start"
          q <- def "goal"
          m <- mapP
          return (p,q,m)

-- mapP :: Parser Maze
-- mapP = M <$> many row

-- file :: Parser Task
-- file = (,,) <$> def "start"
--             <*> def "goal"
--             <*> mapP

solveTask :: Task -> IO ()
solveTask t@(p,q,m) = case solve t of
    Nothing -> putStrLn "No solution exists."
    Just ps -> print $ drawSol m ps

main :: IO ()
main = do 
        print(getBlock (1,0) maze)
        print(getBlock (10,10) maze)
        print(setBlock S (1,2) maze)
        print(nextPos (1,1) maze)
        print(extend [(1,2),(1,1)] maze)
        print(solve ((1,2),(3,3),maze))
        print(solve ((3,1),(3,3),maze))
        print(parse digit "34abc")
        print(parse (string "start") "start = (1,2)")
        print(parse (many (char 'a')) "aaabc")
        print(parse (many (char 'a')) "bc")
        print(parse (some (char 'a')) "aaabc")
        print(parse (some (char 'a')) "bc")
        print(parse (token (char '=')) " = (1,2)")
        print(parse pos "(  343, 55 )")
        print(parse pos "(1 2)")
        print(parse (def "start") "start = (3,2)\n")
        print(parse row "  ### # \n#      #\n")
        str <- getContents
        case parse file str of
              Nothing -> putStrLn "Incorrect task!"
              Just (t, "") -> solveTask t
              Just (_, out) -> putStrLn $ "Unused input: " ++ out
