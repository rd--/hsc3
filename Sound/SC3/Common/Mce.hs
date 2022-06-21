-- | The Sc3 multiple channel expansion (mce) rules over an abstract type.
module Sound.SC3.Common.Mce where

import qualified Sound.SC3.Common.Base {- hsc3 -}

{- | Multiple channel expansion.
     The Mce type is a tree, however in hsc3 Mce_Vector will always hold Mce_Scalar elements.
-}
data Mce t = Mce_Scalar t | Mce_Vector [Mce t]
             deriving (Ord, Eq, Read, Show)

{- | There are two invariants:
     1. Mce should not be empty, ie. Mce_Vector should not have a null list.
     2. Scalar Mce values should not be written as one-place vectors.

> mce_is_well_formed (Mce_Vector []) == False
> mce_is_well_formed (Mce_Vector [Mce_Scalar 1]) == False
-}
mce_is_well_formed :: Mce t -> Bool
mce_is_well_formed m =
  case m of
    Mce_Scalar _ -> True
    Mce_Vector v -> length v > 1 && all mce_is_well_formed v

-- | Is Mce scalar.
mce_is_scalar :: Mce t -> Bool
mce_is_scalar m =
  case m of
    Mce_Scalar _ -> True
    _ -> False

-- | fromList for Mce, generates well-formed Mce.
mce_from_list :: [t] -> Mce t
mce_from_list l =
  case l of
    [] -> error "mce_from_list: null?"
    [e] -> Mce_Scalar e
    _ -> Mce_Vector (map Mce_Scalar l)

{- | toList for Mce.

> let v = Mce_Vector in mce_to_list (v[v[1, 2], 3, v[4, 5]]) == [1, 2, 3, 4, 5]
-}
mce_to_list :: Mce t -> [t]
mce_to_list m =
    case m of
      Mce_Scalar e -> [e]
      Mce_Vector e -> concatMap mce_to_list e

{- | Pretty printer for Mce.

> let v = Mce_Vector in mce_show (v[1, 2, v[3, 4]] * 5 + v[6, 7, 8]) == "[11, 17, [23, 28]]"
-}
mce_show :: Show t => Mce t -> String
mce_show m =
  let bracketed (l,r) x = l : x ++ [r]
  in case m of
       Mce_Scalar e -> show e
       Mce_Vector e -> bracketed ('[',']') (Sound.SC3.Common.Base.concat_intersperse ", " (map mce_show e))

-- | Read value from Mce_Scalar, error if Mce is Mce_Vector
mce_scalar_value :: Mce t -> t
mce_scalar_value m =
  case m of
    Mce_Scalar x -> x
    Mce_Vector _ -> error "mce_scalar_value: not Mce_Scalar"

{- | Length, or perhaps rather width, of Mce.
     Considers only the outermost level, i.e. mce_length is not necessarily the length of mce_to_list.
-}
mce_length :: Mce a -> Int
mce_length m =
  case m of
    Mce_Scalar _ -> 1
    Mce_Vector e -> length e

{- | The depth of an Mce is the longest sequence of nested Mce_Vector nodes.

> mce_depth 1 == 1
> mce_depth (Mce_Vector [1, 2]) == 1
> let v = Mce_Vector in mce_depth (v[v[1, 2], 3, v[4, 5]]) == 2
> let v = Mce_Vector in mce_depth (v[v[1, 2, 3, v[4, 5], 6], 7]) == 3
-}
mce_depth :: Mce a -> Int
mce_depth m =
  case m of
    Mce_Scalar _ -> 1
    Mce_Vector v -> if all mce_is_scalar v then 1 else 1 + maximum (map mce_depth v)

{- | Extend 'Mce' to specified degree.
     Considers only the outermost level.
-}
mce_extend :: Int -> Mce t -> Mce t
mce_extend n m =
    case m of
      Mce_Scalar _ -> Mce_Vector (replicate n m)
      Mce_Vector e -> if length e > n then error "mce_extend?" else Mce_Vector (take n (cycle e))

-- | fmap for Mce, apply /f/ at elements of /m/.
mce_map :: (a -> b) -> Mce a -> Mce b
mce_map f m =
    case m of
      Mce_Scalar e -> Mce_Scalar (f e)
      Mce_Vector e -> Mce_Vector (map (mce_map f) e)

instance Functor Mce where fmap = mce_map

{- | Apply /f/ pairwise at elements of /m1/ and /m2/.
     At each level this extends the shorter of the two operands.
-}
mce_binop :: (a -> b -> c) -> Mce a -> Mce b -> Mce c
mce_binop f m1 m2 =
    case (m1,m2) of
      (Mce_Scalar e1,Mce_Scalar e2) -> Mce_Scalar (f e1 e2)
      (Mce_Scalar _,Mce_Vector e2) -> Mce_Vector (map (mce_binop f m1) e2)
      (Mce_Vector e1,Mce_Scalar _) -> Mce_Vector (map (flip (mce_binop f) m2) e1)
      (Mce_Vector e1,Mce_Vector e2) ->
          let n = max (length e1) (length e2)
              ext = take n . cycle
          in Mce_Vector (zipWith (mce_binop f) (ext e1) (ext e2))

instance Num n => Num (Mce n) where
    (+) = mce_binop (+)
    (-) = mce_binop (-)
    (*) = mce_binop (*)
    abs = mce_map abs
    negate = mce_map negate
    signum = mce_map signum
    fromInteger = Mce_Scalar . fromInteger

instance Fractional n => Fractional (Mce n) where
    (/) = mce_binop (/)
    fromRational = Mce_Scalar . fromRational

instance Floating n => Floating (Mce n) where
  pi = Mce_Scalar pi
  exp = fmap exp
  log = fmap log
  sqrt = fmap sqrt
  (**) = mce_binop (**)
  logBase = mce_binop logBase
  sin = fmap sin
  cos = fmap cos
  asin = fmap asin
  acos = fmap acos
  atan = fmap atan
  sinh = fmap sinh
  cosh = fmap cosh
  asinh = fmap asinh
  acosh = fmap acosh
  atanh = fmap atanh

{-

If UGen is any of Functor, Foldable, Traversable, then Mce must be as well.

{-# Language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

-}
