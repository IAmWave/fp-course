module HM where

import qualified Data.Set as Set

import Types
import TypeSub
import Unification

-- Using a map/hashmap would be better but would require
-- implementing ordering or hashing of terms
type TContext = [(Term, PolyT)]

inferType :: TContext -> Term -> Maybe PolyT
inferType [] _ = Nothing
inferType ((tr,tp):xs) t
    | tr == t       = Just tp
    | otherwise     = inferType xs t

-- Remove all declarations in which the term variable with symbol ts occurs
removeVar :: TContext -> TermSym -> TContext
removeVar g ts = filter (\(tr,tp) -> not $ containsVar tr ts) g
    where
        containsVar :: Term -> TermSym -> Bool
        containsVar (Var v) ts = v == ts
        containsVar (App x y) ts = (containsVar x ts) || (containsVar y ts)
        containsVar (Abs v x) ts = (v == ts) || (containsVar x ts)

-- Return the set of free type variables of a polytype
ftv :: PolyT -> Set.Set TVarSym
ftv (PolyTAbs ts t) = Set.difference (ftv t) (Set.singleton ts)
ftv (PolyTMono tm)  = ftv' tm
    where 
        ftv' :: MonoT -> Set.Set TVarSym
        ftv' (Function t1 t2) = Set.union (ftv' t1) (ftv' t2)
        ftv' (TVar tv)        = Set.singleton tv
        ftv' (TConst _)       = Set.empty

ftvContext :: TContext -> Set.Set TVarSym
ftvContext = Set.unions . (map (ftv.snd))

-- fmap on tuples maps over the second value (here tuples are (type,term))
subContext :: TContext -> TypeSub -> TContext
subContext g sub = map (fmap sub) g

-- Quantify all free type variables in a given type.
generalize :: TContext -> PolyT -> PolyT
generalize g t = quantify t (Set.toList toQuantify)
    where
        toQuantify = Set.difference (ftv t) (ftvContext g)
        quantify :: PolyT -> [TVarSym] -> PolyT
        quantify t [] = t
        quantify t (x:xs) = PolyTAbs x (quantify t xs)

-- Give quantifiers new names to prevent name clashes
renameQuantifiers :: PolyT -> TVarSym -> (TypeSub, TVarSym)
renameQuantifiers (PolyTAbs q p) vi = (s . s', vi')
    where
        s = sub q (TVar vi)
        (s', vi') = renameQuantifiers (s p) (vi+1)
renameQuantifiers (PolyTMono m) vi = (id, vi)

-- Take the context, term and fresh variable index
-- and return (substitution, type of term, new fresh variable index)
w :: TContext -> Term -> TVarSym -> (TypeSub, PolyT, TVarSym)
-- Variable
w g (Var v) vi = case (inferType g (Var v)) of
    Just t  -> (id, f t, vi')
        where
            (f, vi') = renameQuantifiers t vi
            makeFresh = id
    Nothing -> error $ "No type found for " ++ (show (Var v))

-- Abstraction
w g (Abs v tr) vi = (s', PolyTMono$Function freshVar' mt', vi')
    where
        (s',pt',vi') = w g' tr (vi+1)
        -- we assume the returned type is a monotype, which should be the case
        -- since it's the right side of a function
        PolyTMono mt' = pt'
        -- in g', make v have (fresh) type vi
        g' = (Var v,PolyTMono$freshVar):(removeVar g v)
        freshVar = TVar vi
        PolyTMono freshVar'= s' (PolyTMono freshVar)

-- Application 
w g (App tr1 tr2) vi = (u.s2.s1, u $ PolyTMono (TVar vi), vi2)
    where
        -- variable with index vi is used in the main body, so start from vi+1
        (s1, t1, vi1) = w g tr1 (vi+1)
        (s2, t2, vi2) = w (subContext g s1) tr2 vi1
        u = mgu
            (discardQuantifiers (s2 t1))
            (Function (discardQuantifiers t2) (TVar vi))

-- Let expression: let v = tr1 in tr2
w g (Let v tr1 tr2) vi = (s2.s1, t2, vi2)
    where
        (s1, t1, vi1) = w g tr1 vi
        gs1 = subContext g s1
        g2 = (Var v, generalize gs1 t1):(removeVar gs1 v)
        (s2, t2, vi2) = w g2 tr2 vi1

