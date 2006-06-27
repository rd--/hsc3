module Hsc.Random where

import System.Random

choose :: [a] -> IO a
choose l = do n <- getStdRandom (randomR (0,(length l)-1))
              return (l !! n)

randr :: Float -> Float -> IO Float
randr l r = do n <- getStdRandom (randomR (l,r))
               return n

