-- | Common core functions.
module Sound.Sc3.Common.Base where

import Data.Char {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}
import Data.Ord {- base -}

-- * Function

-- | Unary function.
type Fn1 a b = a -> b

-- | Binary function.
type Fn2 a b c = a -> b -> c

-- | Ternary function.
type Fn3 a b c d = a -> b -> c -> d

-- | Quaternary function.
type Fn4 a b c d e = a -> b -> c -> d -> e

-- | 5-parameter function.
type Fn5 a b c d e f = a -> b -> c -> d -> e -> f

-- | 6-parameter function.
type Fn6 a b c d e f g = a -> b -> c -> d -> e -> f -> g

-- | 10-parameter function.
type Fn10 a b c d e f g h i j k = a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k

-- | 11-parameter function.
type Fn11 a b c d e f g h i j k l = a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l

-- | Apply /f/ n times, ie. iterate f x !! n
--
-- > iter 3 (* 2) 1 == 8
-- > iterate (* 2) 1 !! 3 == 8
iter :: Int -> (a -> a) -> a -> a
iter n f x = if n == 0 then x else f (iter (n - 1) f x)

-- * Functor

-- | This is the same function as Control.Monad.void, which however hugs does not know of.
fvoid :: Functor f => f a -> f ()
fvoid = fmap (const ())

-- * Read

-- | Variant of 'reads' requiring exact match.
reads_exact :: Read a => String -> Maybe a
reads_exact s =
    case reads s of
      [(r,"")] -> Just r
      _ -> Nothing

-- * String

{- | Similar to Data.List.Split.splitOn, which however hugs doesn't know of.

> string_split_at_char ':' "/usr/local/bin:/usr/bin:/bin" == ["/usr/local/bin","/usr/bin","/bin"]
> string_split_at_char ':' "/usr/local/bin" == ["/usr/local/bin"]
-}
string_split_at_char :: Char -> String -> [String]
string_split_at_char c s =
  case break (== c) s of
    (lhs,[]) -> [lhs]
    (lhs,_:rhs) -> lhs : string_split_at_char c rhs

-- * String / Case

-- | Ci = Case insensitive, Cs = case sensitive, Sci = separator & case insensitive
data Case_Rule = Ci | Cs | Sci deriving (Eq)

{- | String equality with 'Case_Rule'.

> string_eq Ci "sinOsc" "SinOsc" == True
> string_eq Sci "sin-osc" "SinOsc" == True
-}
string_eq :: Case_Rule -> String -> String -> Bool
string_eq cr x y =
  let ci_form = map toLower
      sci_form = filter (`notElem` "-_") . ci_form
  in case cr of
       Ci -> ci_form x == ci_form y
       Cs -> x == y
       Sci -> sci_form x == sci_form y

-- | 'rlookup_by' of 'string_eq'.
rlookup_str :: Case_Rule -> String -> [(a,String)] -> Maybe a
rlookup_str = rlookup_by . string_eq

{- | 'Enum' parser with 'Case_Rule'.

> parse_enum Ci "false" == Just False
-}
parse_enum :: (Show t,Enum t,Bounded t) => Case_Rule -> String -> Maybe t
parse_enum cr nm =
    let u = [minBound .. maxBound]
        t = zip (map show u) u
    in lookup_by (string_eq cr) nm t

-- * List

-- | Left to right composition of a list of functions.
--
-- > compose_l [(* 2),(+ 1)] 3 == 7
compose_l :: [t -> t] -> t -> t
compose_l = flip (foldl (\x f -> f x))

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

-- | !! with localised error message
at_with_error_message :: String -> [t] -> Int -> t
at_with_error_message msg list index =
  if index >= length list
  then error ("!!: index out of range: " ++ msg)
  else list !! index

-- | concat of intersperse.  This is the same function as intercalate, which hugs doesn't know of.
concat_intersperse :: [a] -> [[a]] -> [a]
concat_intersperse x = concat . intersperse x

{- | Similar to Data.List.Split.splitOn, which however hugs doesn't know of.

> list_split_at_elem ' ' "a sequence of words" == ["a","sequence","of","words"]
-}
list_split_at_elem :: Eq t => t -> [t] -> [[t]]
list_split_at_elem c s =
  case break (== c) s of
    (lhs,[]) -> [lhs]
    (lhs,_:rhs) -> lhs : list_split_at_elem c rhs

{- | Data.List.sortOn, which however hugs does not know of.

> sort_on snd [('a',1),('b',0)] == [('b',0),('a',1)]
-}
sort_on :: (Ord b) => (a -> b) -> [a] -> [a]
sort_on = sortBy . comparing

{- | Inserts at the first position where it compares less but not equal to the next element.

> import Data.Function {- base -}
> insertBy (compare `on` fst) (3,'x') (zip [1..5] ['a'..])
> insertBy_post (compare `on` fst) (3,'x') (zip [1..5] ['a'..])
-}
insertBy_post :: (a -> a -> Ordering) -> a -> [a] -> [a]
insertBy_post cmp e l =
    case l of
      [] -> [e]
      h:l' -> case cmp e h of
                LT -> e : l
                _ -> h : insertBy_post cmp e l'

-- | 'insertBy_post' using 'compare'.
insert_post :: Ord t => t -> [t] -> [t]
insert_post = insertBy_post compare

-- | Apply /f/ at all but last element, and /g/ at last element.
--
-- > at_last (* 2) negate [1..4] == [2,4,6,-4]
at_last :: (a -> b) -> (a -> b) -> [a] -> [b]
at_last f g x =
    case x of
      [] -> []
      [i] -> [g i]
      i:x' -> f i : at_last f g x'

-- * Tuples

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
