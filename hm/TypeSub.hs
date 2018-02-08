module TypeSub where

import Types

-- We model type substitutions as functions; composing substitutions
-- is now simply composing functions
type TypeSub = PolyT -> PolyT

-- Replace all occurrences of type variable symbol `f` with type `t`
sub :: TVarSym -> MonoT -> TypeSub
sub f t (PolyTAbs q p)
    | q == f    = PolyTAbs tq (sub f t p)
    | otherwise = PolyTAbs f (sub f t p)
        where
            -- Quantifiers must only be substituted for variables.
            TVar tq = t

sub f t (PolyTMono m) = PolyTMono (subMono f t m)
    where
        subMono :: TVarSym -> MonoT -> MonoT -> MonoT
        subMono f t (Function m1 m2)
            = Function (subMono f t m1) (subMono f t m2)
        subMono f t (TConst c) = TConst c
        subMono f t (TVar v)
            | f == v   = t
            | otherwise = TVar v
