-- | Common 'Control.Monad' variations.
module Sound.SC3.Common.Monad where

-- | 'sequence' of 'repeat'
repeatM :: Monad m => m t -> m [t]
repeatM = sequence . repeat

-- | This is the same function as Control.Monad.void, which however hugs does not know of.
mvoid :: Monad m => m a -> m ()
mvoid f = f >> return ()

-- | 'void' of 'repeatM'.
repeatM_ :: Monad m => m t -> m ()
repeatM_ = mvoid . repeatM

{- | Right to left compositon of 'Monad' functions.

> fmap (== 7) (composeM [return . (+ 1),return . (* 2)] 3)
> fmap (== 8) (composeM [return . (* 2),return . (+ 1)] 3)
-}
composeM :: Monad m => [a -> m a] -> a -> m a
composeM = foldr (\f g -> \x -> g x >>= f) return -- foldr (<=<) return except hugs doesn't know of <=<

{- | Feed forward composition of /n/ applications of /f/.

> fmap (== 3) (chainM 3 (return . (+ 1)) 0)
-}
chainM :: Monad m => Int -> (b -> m b) -> b -> m b
chainM n f = composeM (replicate n f)
