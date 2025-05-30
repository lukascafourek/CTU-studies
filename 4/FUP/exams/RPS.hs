module RPS (rps) where
import Data.List ( nub, sort )

isFinished xs = null xs || any null xs

currentStrategies = map head

futureStrategies = map tail

view :: [b] -> [Bool] -> [b]
view players bools = map fst $ filter snd $ zip players bools

eliminate :: [Char] -> [Bool]
eliminate strats = let el = elim $ sort $ nub strats
                   in map (/= el) strats
                   where elim ['r', 's'] = 's'
                         elim ['p', 'r'] = 'r'
                         elim ['p', 's'] = 'p'
                         elim _ = ' '

rps :: [String] -> [[Char]] -> [String]
rps players strategies | isFinished strategies = players
rps players strategies =
    let current = currentStrategies strategies
        future = futureStrategies strategies
        round = eliminate current
    in rps (view players round) (view future round)
