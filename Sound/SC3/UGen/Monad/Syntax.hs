module Sound.SC3.UGen.Monad.Syntax where

import Control.Monad

infixl 7  .*,*.,.*.
infixl 6  .+,+.,.+.

-- | Infix 'fmap'.
--
-- > filter even # return [1..5]
(#) :: Functor f => (a -> b) -> f a -> f b
(#) = fmap

-- | '+' variant with 'Functor' at left.
--
-- > fmap (== 5) (return 3 .+ 2)
(.+) :: (Functor f, Num a) => f a -> a -> f a
m .+ n = fmap (+ n) m

-- | '+' variant with 'Functor' at right.
--
-- > fmap (== 5) (3 +. return 2)
(+.) :: (Functor f, Num a) => a -> f a -> f a
n +. m = fmap (+ n) m

-- | '+' variant with 'Monad' at left and right.
--
-- > fmap (== 5) (return 3 .+. return 2)
(.+.) :: (Monad m, Num a) => m a -> m a -> m a
(.+.) = liftM2 (+)

-- | '*' variant with 'Functor' at left.
--
-- > fmap (== 6) (return 3 .* 2)
(.*) :: (Functor f, Num a) => f a -> a -> f a
m .* n = fmap (* n) m

-- | '*' variant with 'Functor' at right.
--
-- > fmap (== 6) (3 *. return 2)
(*.) :: (Functor f, Num a) => a -> f a -> f a
n *. m = fmap (* n) m

-- | '*' variant with 'Monad' at left and right.
--
-- > fmap (== 6) (return 3 .*. return 2)
(.*.) :: (Monad m, Num a) => m a -> m a -> m a
(.*.) = liftM2 (*)
