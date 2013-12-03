-- {-# LANGUAGE StandaloneDeriving, MultiParamTypeClasses, FlexibleInstances,
--              KindSignatures, ConstraintKinds, UndecidableInstances, FlexibleContexts #-}
-- module ClassConstraint where
-- 
-- import GHC.Exts (Constraint)
-- 
-- data MyData = MyData deriving (Eq, Show)
-- deriving instance Read MyData
-- 
-- class (cls :: * -> Constraint) (ty :: *) => Derived cls ty
-- instance Derived Eq MyData
-- instance Derived Eq Int
-- 
-- safeEq :: (Derived Eq a) => a -> a -> Bool
-- safeEq = (==)

{-# LANGUAGE StandaloneDeriving, MultiParamTypeClasses, KindSignatures,
             FlexibleContexts #-}
module ClassConstraint where

import GHC.Exts (Constraint)

data MyData = MyData deriving (Eq, Show)
deriving instance Read MyData
 
class Derived (cls :: * -> Constraint) (ty :: *)
instance Derived Eq MyData
instance Derived Eq Int

safeEq :: (Derived Eq a, Eq a) => a -> a -> Bool
safeEq = (==)

