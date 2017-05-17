module Sound.SC3.Common.Prelude where

import Data.Char {- base -}
import Data.List {- base -}

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

-- * List

-- > d_dx [0,1,3,6] == [0,1,2,3]
d_dx :: (Num a) => [a] -> [a]
d_dx l = zipWith (-) l (0:l)

-- > dx_d (d_dx [0,1,3,6]) == [0,1,3,6]
-- > dx_d [0.5,0.5] == [0.5,1]
dx_d :: Num n => [n] -> [n]
dx_d = scanl1 (+)

-- > d_dx' [0,1,3,6] == [1,2,3]
d_dx' :: Num n => [n] -> [n]
d_dx' l = zipWith (-) (tail l) l

-- > dx_d' (d_dx' [0,1,3,6]) == [0,1,3,6]
-- > dx_d' [0.5,0.5] == [0,0.5,1]
dx_d' :: Num n => [n] -> [n]
dx_d' = (0 :) . scanl1 (+)

-- | 'lookup' with equality function.
lookup_by :: (a -> a -> Bool) -> a -> [(a,b)] -> Maybe b
lookup_by f x = fmap snd . find (f x . fst)

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

-- * TUPLES

type T2 a = (a,a)
type T3 a = (a,a,a)
type T4 a = (a,a,a,a)

-- | 'concatMap' of /f/ at /x/ and /g/ at /y/.
mk_duples :: (a -> c) -> (b -> c) -> [(a, b)] -> [c]
mk_duples a b = concatMap (\(x,y) -> [a x, b y])

-- | Length prefixed list variant of 'mk_duples'.
mk_duples_l :: (Int -> c) -> (a -> c) -> (b -> c) -> [(a,[b])] -> [c]
mk_duples_l i a b = concatMap (\(x,y) -> a x : i (length y) : map b y)

-- | 'concatMap' of /f/ at /x/ and /g/ at /y/ and /h/ at /z/.
mk_triples :: (a -> d) -> (b -> d) -> (c -> d) -> [(a, b, c)] -> [d]
mk_triples a b c = concatMap (\(x,y,z) -> [a x, b y, c z])
