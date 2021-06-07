-- | Common core functions.
module Sound.SC3.Common.Base where

import Control.Exception {- base -}
import Control.Monad {- base -}
import Data.Char {- base -}
import Data.Function {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}
import System.IO.Error {- base -}
import System.Environment {- base -}

-- * Function

-- | Unary function.
type Fn1 a b = a -> b

-- | Binary function.
type Fn2 a b c = a -> b -> c

-- | Ternary function.
type Fn3 a b c d = a -> b -> c -> d

-- | Quaternary function.
type Fn4 a b c d e = a -> b -> c -> d -> e

-- | Apply /f/ n times, ie. iterate f x !! n
--
-- > iter 3 (* 2) 1 == 8
-- > iterate (* 2) 1 !! 3 == 8
iter :: Int -> (a -> a) -> a -> a
iter n f x = if n == 0 then x else f (iter (n - 1) f x)

-- * Read

-- | Variant of 'reads' requiring exact match.
reads_exact :: Read a => String -> Maybe a
reads_exact s =
    case reads s of
      [(r,"")] -> Just r
      _ -> Nothing

-- * STRING / CASE

-- | CI = Case insensitive, CS = case sensitive.
data Case_Rule = CI | CS deriving (Eq)

-- | Predicates for 'Case_Rule'.
is_ci :: Case_Rule -> Bool
is_ci = (==) CI

-- | Predicates for 'Case_Rule'.
is_cs :: Case_Rule -> Bool
is_cs = (==) CS

-- | String equality with 'Case_Rule'.
--
-- > string_eq CI "lower" "LOWER" == True
string_eq :: Case_Rule -> String -> String -> Bool
string_eq cr x y = if is_ci cr then map toLower x == map toLower y else x == y

-- | 'rlookup_by' of 'string_eq'.
rlookup_str :: Case_Rule -> String -> [(a,String)] -> Maybe a
rlookup_str = rlookup_by . string_eq

-- | 'Enum' parser with 'Case_Rule'.
--
-- > parse_enum CI "FALSE" == Just False
parse_enum :: (Show t,Enum t,Bounded t) => Case_Rule -> String -> Maybe t
parse_enum cr nm =
    let u = [minBound .. maxBound]
        t = zip (map show u) u
    in lookup_by (string_eq cr) nm t

-- * LIST

-- | Left to right composition of a list of functions.
--
-- > compose_l [(* 2),(+ 1)] 3 == 7
compose_l :: [t -> t] -> t -> t
compose_l = flip (foldl (&))

-- | Right to left composition of a list of functions.
--
-- > compose_r [(* 2),(+ 1)] 3 == 8
compose_r :: [t -> t] -> t -> t
compose_r = flip (foldr ($))

{- | SequenceableCollection.differentiate

> > [3,4,1,1].differentiate == [3,1,-3,0]

> d_dx [3,4,1,1] == [3,1,-3,0]
> d_dx [0,1,3,6] == [0,1,2,3]
-}
d_dx :: (Num a) => [a] -> [a]
d_dx l = zipWith (-) l (0:l)

{- | Variant that does not prepend zero to input, ie. 'tail' of 'd_dx'.

> d_dx' [3,4,1,1] == [1,-3,0]
> d_dx' [0,1,3,6] == [1,2,3]
-}
d_dx' :: Num n => [n] -> [n]
d_dx' l = zipWith (-) (tail l) l

{- | SequenceableCollection.integrate

> > [3,4,1,1].integrate == [3,7,8,9]

> dx_d [3,4,1,1] == [3,7,8,9]
> dx_d (d_dx [0,1,3,6]) == [0,1,3,6]
> dx_d [0.5,0.5] == [0.5,1]
-}
dx_d :: Num n => [n] -> [n]
dx_d = scanl1 (+)

{- | Variant pre-prending zero to output.

> dx_d' [3,4,1,1] == [0,3,7,8,9]
> dx_d' (d_dx' [0,1,3,6]) == [0,1,3,6]
> dx_d' [0.5,0.5] == [0,0.5,1]
-}
dx_d' :: Num n => [n] -> [n]
dx_d' = (0 :) . dx_d

-- | 'lookup' with equality function.
lookup_by :: (a -> t -> Bool) -> a -> [(t,b)] -> Maybe b
lookup_by f x = fmap snd . find (f x . fst)

-- | Erroring variant.
lookup_by_err :: (a -> t -> Bool) -> a -> [(t,b)] -> b
lookup_by_err f x = fromMaybe (error "lookup_by") . lookup_by f x

-- | Reverse 'lookup' with equality function.
rlookup_by :: (b -> b -> Bool) -> b -> [(a,b)] -> Maybe a
rlookup_by f x = fmap fst . find (f x . snd)

-- | (prev,cur,next) triples.
--
-- > pcn_triples [1..3] == [(Nothing,1,Just 2),(Just 1,2,Just 3),(Just 2,3,Nothing)]
pcn_triples :: [a] -> [(Maybe a,a,Maybe a)]
pcn_triples =
    let f e l = case l of
                  e1 : e2 : l' -> (e,e1,Just e2) : f (Just e1) (e2 : l')
                  [e'] -> [(e,e',Nothing)]
                  [] -> undefined
    in f Nothing

-- | Separate first list element.
--
-- > sep_first "astring" == Just ('a',"string")
sep_first :: [t] -> Maybe (t,[t])
sep_first l =
    case l of
      e:l' -> Just (e,l')
      _ -> Nothing

-- | Separate last list element.
--
-- > sep_last "stringb" == Just ("string",'b')
sep_last :: [t] -> Maybe ([t], t)
sep_last =
    let f (e,l) = (reverse l,e)
    in fmap f . sep_first . reverse

-- | Are lists of equal length?
--
-- > equal_length_p ["t1","t2"] == True
-- > equal_length_p ["t","t1","t2"] == False
equal_length_p :: [[a]] -> Bool
equal_length_p = (== 1) . length . nub . map length

-- | Histogram
histogram :: Ord a => [a] -> [(a,Int)]
histogram x =
    let g = group (sort x)
    in zip (map head g) (map length g)

-- * TUPLES

-- | Zip two 4-tuples.
p4_zip :: (a,b,c,d) -> (e,f,g,h) -> ((a,e),(b,f),(c,g),(d,h))
p4_zip (a,b,c,d) (e,f,g,h) = ((a,e),(b,f),(c,g),(d,h))

-- | Two-tuple.
type T2 a = (a,a)

-- | Three-tuple.
type T3 a = (a,a,a)

-- | Four-tuple.
type T4 a = (a,a,a,a)

-- | t -> (t,t)
dup2 :: t -> T2 t
dup2 t = (t,t)

-- | t -> (t,t,t)
dup3 :: t -> T3 t
dup3 t = (t,t,t)

-- | t -> (t,t,t,t)
dup4 :: t -> T4 t
dup4 t = (t,t,t,t)

-- | 'concatMap' of /f/ at /x/ and /g/ at /y/.
mk_duples :: (a -> c) -> (b -> c) -> [(a, b)] -> [c]
mk_duples a b = concatMap (\(x,y) -> [a x, b y])

-- | Length prefixed list variant of 'mk_duples'.
mk_duples_l :: (Int -> c) -> (a -> c) -> (b -> c) -> [(a,[b])] -> [c]
mk_duples_l i a b = concatMap (\(x,y) -> a x : i (length y) : map b y)

-- | 'concatMap' of /f/ at /x/ and /g/ at /y/ and /h/ at /z/.
mk_triples :: (a -> d) -> (b -> d) -> (c -> d) -> [(a, b, c)] -> [d]
mk_triples a b c = concatMap (\(x,y,z) -> [a x, b y, c z])

-- | [x,y] -> (x,y)
t2_from_list :: [t] -> T2 t
t2_from_list l = case l of {[p,q] -> (p,q);_ -> error "t2_from_list"}

-- | [x,y,z] -> (x,y,z)
t3_from_list :: [t] -> (t,t,t)
t3_from_list l = case l of {[p,q,r] -> (p,q,r);_ -> error "t3_from_list"}

-- * System

-- | Guarded variant of 'getEnv' with default value.
get_env_default :: String -> String -> IO String
get_env_default e k = do
  r <- tryJust (guard . isDoesNotExistError) (getEnv e)
  case r of
    Right v -> return v
    _ -> return k

-- | 'lookupEnv' with default value.
--
-- > lookup_env_default "PATH" "/usr/bin"
lookup_env_default :: String -> String -> IO String
lookup_env_default e k = fmap (fromMaybe k) (lookupEnv e)
