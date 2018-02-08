module Types where

-- Terms
type TermSym = Integer
data Term = Var TermSym |
            App Term Term |
            Abs TermSym Term |      
            Let TermSym Term Term   -- Let TermSym = Term1 in Term2
    deriving (Eq, Show)


type TVarSym = Integer      -- Variable identifier
type TConstSym = Integer    -- Constant identifier
data MonoT = TConst TConstSym |
             TVar TVarSym |
             Function MonoT MonoT
    deriving (Eq)
data PolyT = PolyTAbs TVarSym PolyT | PolyTMono MonoT
    deriving (Eq)

instance Show MonoT where
    show (TConst n) = "τ" ++ show n
    show (TVar n) = "α" ++ show n
    show (Function a b) = "(" ++ (show a) ++ "->" ++ (show b) ++ ")"

instance Show PolyT where
    show (PolyTMono m) = show m
    show (PolyTAbs v m) = "∀" ++ (show (TVar v)) ++ "." ++ (show m)

discardQuantifiers :: PolyT -> MonoT
discardQuantifiers (PolyTMono m) = m
discardQuantifiers (PolyTAbs _ p)= discardQuantifiers p
