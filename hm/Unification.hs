module Unification where

import Types
import TypeSub

type Agenda = [(MonoT, MonoT)]

-- Return the most general unification of t1 and t2.
-- Raises an error if the MGU does not exist.

mgu :: MonoT -> MonoT -> TypeSub
mgu t1 t2 = mgu' [(t1,t2)] id

mgu' :: Agenda -> TypeSub -> TypeSub
mgu' [] r = r
mgu' ((t1,t2):ag) r = mgu' ag' (r'.r)
    where
        (ag', r') = mguProcess t1 t2 ag

mguProcess :: MonoT -> MonoT -> Agenda -> (Agenda, TypeSub)
mguProcess (Function t11 t12) (Function t21 t22) ag
    = ((t11,t21):(t12,t22):ag, id)
mguProcess (TConst a) (TConst b) ag
    | a == b    = (ag,id)
    | otherwise = error $ "Type constants " ++ (show a) ++ " and " ++ (show b) ++ " do not match"
mguProcess (TVar a) tb ag = mguProcessTVar a tb ag
mguProcess ta (TVar b) ag = mguProcessTVar b ta ag
mguProcess t1 t2 ag = error $ "Types " ++ (show t1) ++ " and " ++ (show t2) ++ " could not be unified"

mguProcessTVar :: TVarSym -> MonoT -> Agenda -> (Agenda, TypeSub)
mguProcessTVar ts t ag
    | containsTVar t ts = error $ (show t) ++ " contains " ++ (show ts)
    | otherwise         = (ag', s)
    where
        ag' = map (\(x, y) -> (ms x, ms y)) ag
        ms x = sx
            where PolyTMono sx = s (PolyTMono x)
        s = sub ts t

containsTVar :: MonoT -> TVarSym -> Bool
containsTVar (TConst _)     v = False
containsTVar (TVar x)       v = v == x
containsTVar (Function x y) v = (containsTVar x v) || (containsTVar y v)
