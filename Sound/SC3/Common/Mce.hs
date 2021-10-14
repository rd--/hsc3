-- | The SC3 multiple channel expansion rules over an abstract type.
module Sound.SC3.Common.Mce where

-- | Multiple channel expansion.
data Mce t = Mce_Unit t | Mce_Vector [t]
             deriving (Ord, Eq, Read, Show)

-- | fromList for Mce.
mce_from_list :: [t] -> Mce t
mce_from_list = Mce_Vector

-- | toList for Mce.
mce_elem :: Mce t -> [t]
mce_elem m =
    case m of
      Mce_Unit e -> [e]
      Mce_Vector e -> e

-- | Length, or perhaps rather width, of Mce.
mce_length :: Mce a -> Int
mce_length = length . mce_elem

-- | Extend 'Mce' to specified degree.
mce_extend :: Int -> Mce n -> Mce n
mce_extend n m =
    case m of
      Mce_Unit e -> Mce_Vector (replicate n e)
      Mce_Vector e -> if length e > n then error "mce_extend?" else Mce_Vector (take n (cycle e))

-- | fmap for Mce, apply /f/ at elements of /m/.
mce_map :: (a -> b) -> Mce a -> Mce b
mce_map f m =
    case m of
      Mce_Unit e -> Mce_Unit (f e)
      Mce_Vector e -> Mce_Vector (map f e)

instance Functor Mce where fmap = mce_map

-- | Apply /f/ pairwise at elements of /m1/ and /m2/.
mce_binop :: (a -> b -> c) -> Mce a -> Mce b -> Mce c
mce_binop f m1 m2 =
    case (m1,m2) of
      (Mce_Unit e1,Mce_Unit e2) -> Mce_Unit (f e1 e2)
      (Mce_Unit e1,Mce_Vector e2) -> Mce_Vector (map (f e1) e2)
      (Mce_Vector e1,Mce_Unit e2) -> Mce_Vector (map (`f` e2) e1)
      (Mce_Vector e1,Mce_Vector e2) ->
          let n = max (length e1) (length e2)
              ext = take n . cycle
          in Mce_Vector (zipWith f (ext e1) (ext e2))

instance Num n => Num (Mce n) where
    (+) = mce_binop (+)
    (-) = mce_binop (-)
    (*) = mce_binop (*)
    abs = mce_map abs
    negate = mce_map negate
    signum = mce_map signum
    fromInteger = Mce_Unit . fromInteger

instance Fractional n => Fractional (Mce n) where
    (/) = mce_binop (/)
    fromRational = Mce_Unit . fromRational

{-

If UGen is any of Functor, Foldable, Traversable, then Mce must be as well.

{-# Language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

Should Mce be a Tree?
import qualified Data.Tree as Tree {- containers -}
type Mce t = Tree.Tree [t]

-}
