-- I used the following to better understand De Bruijn indices:
-- https://github.com/Gabriel439/Haskell-Morte-Library/issues/1
-- The implemention follows the one described on the site,
-- especially the shifting.
module Lambda where

import Data.Maybe

data Lambda = Var Integer | Application Lambda Lambda | Abstraction Lambda

instance Show Lambda where
    show x = show' None x

-- The `show` implementation attempts to avoid unnecessary parentheses.
-- To do this, we maintain the context (one level up)
data ShowContext = None | AppL | AppR

show' :: ShowContext -> Lambda -> String
show' AppL (Abstraction a)  = withParens $ "λ " ++ (show' None a)
show' AppR (Abstraction a)  = withParens $ "λ " ++ (show' None a)
show' None (Abstraction a)  =              "λ " ++ (show' None a)
show' AppR  (Application a b) = withParens $ (show' AppL a) ++ " " ++ (show' AppR b)
show' _     (Application a b) =              (show' AppL a) ++ " " ++ (show' AppR b)
show' _ (Var n) = show n

withParens :: String -> String
withParens s = "(" ++ s ++ ")"


instance Eq Lambda where
    (/=) a b = not (a == b)
    (==) (Var a') (Var b') = a' == b'
    (==) (Application a1 a2) (Application b1 b2) = (a1 == b1) && (a2 == b2)
    (==) (Abstraction a') (Abstraction b') = a' == b'


-- uses the normal reduction strategy
reduce :: Lambda -> Lambda
reduce a = case (step a) of
    Just a' -> reduce a'
    Nothing -> a


step :: Lambda -> Maybe Lambda
step (Var a)           = Nothing
step (Application (Abstraction a) b) = Just (betaReduction a b)
step (Abstraction a)   = fmap Abstraction (step a) -- wrap in Abstraction if not Nothing
step (Application a b) = case (step a) of
    Just a' -> Just (Application a' b)
    Nothing -> case (step b) of
        Just b' -> Just (Application a b')
        Nothing -> Nothing


betaReduction :: Lambda -> Lambda -> Lambda
betaReduction a b = shift (apply a 1 b) (-1) 0

-- Perform an application without renumbering anything
apply :: Lambda -> Integer -> Lambda -> Lambda
apply (Var a) depth y
    | a == depth = shift y depth depth
    | otherwise  = Var a
apply (Abstraction a) depth y   = Abstraction (apply a (depth+1) y)
apply (Application a b) depth y = Application (apply a depth y) (apply b depth y)


shift :: Lambda -> Integer -> Integer -> Lambda
shift (Var a) by depth
    | a <= depth    = Var a
    | otherwise     = Var (a + by)
shift (Abstraction a) by depth   = Abstraction (shift a by (depth+1))
shift (Application a b) by depth = Application (shift a by depth) (shift b by depth)
