-- | Functions to make writing 'Applicative' and 'Monad' UGen graphs
-- less clumsy.
module Sound.SC3.Common.Monad.Operators where

import Control.Applicative {- base -}

infixl 7  .*,*.,.*.
infixl 6  .+,+.,.+.

infixl 7  ./,/.,./.
infixl 6  .-,-.,.-.

-- | '+' variant with 'Functor' at left.
--
-- > fmap (== 5) (return 3 .+ 2)
-- > [3,4] .+ 2 == [5,6]
(.+) :: (Functor f, Num a) => f a -> a -> f a
m .+ n = fmap (+ n) m

-- | '+' variant with 'Functor' at right.
--
-- > fmap (== 5) (3 +. return 2)
-- > 3 +. [2,3] == [5,6]
(+.) :: (Functor f, Num a) => a -> f a -> f a
m +. n = fmap (+ m) n

-- | '+' variant with 'Applicative' at left and right.
--
-- > fmap (== 5) (return 3 .+. return 2)
-- > [3,4] .+. [2,3] == [5,6,6,7]
-- > getZipList (ZipList [3,4] .+. ZipList [2,3]) == [5,7]
(.+.) :: (Applicative m, Num a) => m a -> m a -> m a
(.+.) = liftA2 (+)

-- | '*' variant with 'Functor' at left.
--
-- > fmap (== 6) (return 3 .* 2)
(.*) :: (Functor f, Num a) => f a -> a -> f a
m .* n = fmap (* n) m

-- | '*' variant with 'Functor' at right.
--
-- > fmap (== 6) (3 *. return 2)
(*.) :: (Functor f, Num a) => a -> f a -> f a
m *. n = fmap (* m) n

-- | '*' variant with 'Applicative' at left and right.
--
-- > fmap (== 6) (return 3 .*. return 2)
(.*.) :: (Applicative m, Num a) => m a -> m a -> m a
(.*.) = liftA2 (*)

-- | '-' variant with 'Functor' at left.
--
-- > fmap (== 1) (return 3 .- 2)
-- > [3,4] .- 2 == [1,2]
(.-) :: (Functor f, Num a) => f a -> a -> f a
m .- n = fmap (subtract n) m

-- | '-' variant with 'Functor' at right.
--
-- > fmap (== 1) (3 -. return 2)
-- > 3 -. [2,3] == [1,0]
(-.) :: (Functor f, Num a) => a -> f a -> f a
m -. n = fmap (m -) n

-- | '-' variant with 'Applicative' at left and right.
--
-- > fmap (== 1) (return 3 .-. return 2)
-- > [3,4] .-. [2,3] == [1,0,2,1]
-- > getZipList (ZipList [3,4] .-. ZipList [2,3]) == [1,1]
(.-.) :: (Applicative m, Num a) => m a -> m a -> m a
(.-.) = liftA2 (-)

-- | '/' variant with 'Functor' at left.
--
-- > fmap (== 3) (return 6 ./ 2)
(./) :: (Functor f,Fractional a) => f a -> a -> f a
m ./ n = fmap (/ n) m

-- | '/' variant with 'Functor' at right.
--
-- > fmap (== 3) (6 /. return 2)
(/.) :: (Functor f,Fractional a) => a -> f a -> f a
m /. n = fmap (m /) n

-- | '/' variant with 'Applicative' at left and right.
--
-- > fmap (== 3) (return 6 ./. return 2)
-- > [5,6] ./. [2,3] == [5/2,5/3,3,2]
(./.) :: (Applicative m,Fractional a) => m a -> m a -> m a
(./.) = liftA2 (/)
