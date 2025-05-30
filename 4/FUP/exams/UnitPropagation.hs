module Task4 ( propagateUnits, Literal (..) ) where
import Data.List

type Variable = String
data Literal = Neg { variable :: Variable }
             | Pos { variable :: Variable } deriving (Eq, Ord)

type Clause = [Literal]

instance Show Literal where
    show (Neg x) = "-" ++ x
    show (Pos x) = x

negation :: Literal -> Literal
negation (Pos x) = Neg x
negation (Neg x) = Pos x

getUnit :: [Clause] -> Maybe Literal
getUnit cls = case units of
                [] -> Nothing
                (u:us) -> Just u
    where units = concat $ filter ((1==) . length) cls

simplifyClause :: Literal -> Clause -> [Clause]
simplifyClause l c | l `elem` c = []
                   | negation l `elem` c = [delete (negation l) c]
                   | otherwise = [c]

simplify :: [Clause] -> Literal -> [Clause]
simplify cls l = concatMap (simplifyClause l) cls

propagateUnits :: [Clause] -> [Clause]
propagateUnits cls = let mu = getUnit cls
                     in case mu of
                         Nothing -> nub cls
                         Just u -> propagateUnits $ simplify cls u 

dpll :: [Clause] -> Bool
dpll cls = case cls' of
            [] -> True
            []:_ -> False
            (l:_):_ -> dpll (simplify cls' l) || dpll (simplify cls' (negation l))
    where cls' = sort $ propagateUnits cls                  
