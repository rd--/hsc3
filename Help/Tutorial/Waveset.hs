{- A simple waveset synthesiser (rd) -}

import Control.Monad
import Data.Array
import qualified Data.Array.Storable as A
import Data.List
import qualified Sound.File.Sndfile as F
import Sound.OpenSoundControl
import Sound.SC3
import System.Environment
import System.Random

-- * Sound file utilities.

-- | file-name -> (channel-count, frame-count, sample-rate)
sf_info :: String -> IO (Int, Integer, Double)
sf_info fn = do
  h <- F.openFile fn F.ReadMode F.defaultInfo
  let i = F.hInfo h
      nc = F.channels i
      nf = F.frames i
      sr = F.samplerate i
  F.hClose h
  return (nc, fromIntegral nf, fromIntegral sr)

-- | channel-count -> channel -> interleaved-data -> channel-data
extract_channel :: Int -> Int -> [a] -> [a]
extract_channel _ _ [] = []
extract_channel nc n xs =
    let r = extract_channel nc n (drop nc xs)
    in (xs !! n) : r

-- | file-name -> channel -> data
sf_channel :: String -> Int -> IO [Double]
sf_channel fn n = do
  h <- F.openFile fn F.ReadMode F.defaultInfo
  let i = F.hInfo h
      nc = F.channels i
      nf = F.frames i
      ns = nc * nf
  b <- A.newArray_ (0, ns) :: IO (A.StorableArray F.Index Double)
  F.hGetSamples h b ns
  e <- A.getElems b
  return (extract_channel nc n e)

-- * Score model.

-- | Interval to schedule in advance.
latency :: Double
latency = 0.15

-- | Add t to timestamp.
offset :: Double -> OSC -> OSC
offset t (Bundle (UTCr t0) m) = Bundle (UTCr (t + t0)) m
offset _ _ = undefined

-- | Play non-empty set of osc bundles.
play_set :: Transport t => t -> [OSC] -> IO ()
play_set _ [] = undefined
play_set fd (x:xs) = do
  let (Bundle (UTCr t) _) = x
  pauseThreadUntil (t - latency)
  mapM_ (\e -> send fd e) (x:xs)

-- | Play grouped score.
play_sets :: Transport t => t -> [[OSC]] -> IO ()
play_sets _ [] = return ()
play_sets fd s = do
  t <- utcr
  mapM_ (play_set fd) (map (\g -> map (offset t) g) s)

-- | Split l into chunks of at most n elements.
form_sets :: Int -> [a] -> [[a]]
form_sets _ [] = []
form_sets n l =
    let (a,b) = splitAt n l
    in a : form_sets n b

-- | Play score, send in sets on indicated cardinality.
play_score :: Transport t => Int -> t -> [OSC] -> IO ()
play_score n fd s = play_sets fd (form_sets n s)

-- * Waveset analysis

-- | Zero-crossing predicate.
is_zc :: (Num a, Ord a) => a -> a -> Bool
is_zc a b = a <= 0 && b > 0

-- | Locate fractional zero-crossing point.
locate_fzc :: (Ord a, Fractional a) => a -> a -> a
locate_fzc x y = (1.0 / (y - x)) * abs x

-- | Fractional zero-crossing constructor, n is the initial frame location.
fzc :: (Ord a, Fractional a) => a -> [a] -> [a]
fzc _ [] = []
fzc _ [_] = []
fzc n (x:y:z) = if is_zc x y
                then (n + locate_fzc x y) : fzc (n + 2.0) z
                else fzc (n + 1.0) (y : z)

-- | Remove zero crossings so that no waveset has length less than m.
prune :: (Ord a, Num a) => a -> a -> [a] -> [a]
prune _ _ [] = [] -- hmmm....
prune m n (x:xs) = if (x - n) < m
                   then prune m n xs
                   else x : prune m x xs

-- | zc -> ws
ws :: [a] -> [(a,a)]
ws [] = []
ws [_] = []
ws (x:y:z) = (x,y) : ws (y : z)

-- * Waveset instrument

-- | A trivial waveset instrument with unit envelope.
waveset :: UGen
waveset =
    let k = Control KR
        o = k "out" 0
        b = k "bufnum" 0
        s = k "start" 0
        e = k "end" 0
        r = k "rate" 1
        d = k "dur" 1
        a = k "amp" 0.2
        rs = bufRateScale KR b * r
        ph = phasor AR 0 rs 0 (e - s) 0 + s
        e_data = env [a, a, 0] [d, 0] [EnvLin] (-1) (-1)
        e_ugen = envGen AR 1 1 0 1 RemoveSynth e_data
    in offsetOut o (bufRdL 1 AR b ph Loop * e_ugen)

-- * Waveset synthesizer

-- | Construct s_new message for synthesiser.
mk_msg :: Double -> Double -> Double -> Double -> OSC
mk_msg b sf ef d =
    let a = [("bufnum", b), ("start", sf), ("end", ef), ("dur", d)]
    in s_new "waveset" (-1) AddToTail 1 a

-- | Compare wavesets by duration.
dur_ord :: (Num t, Ord t) => (t, t) -> (t, t) -> Ordering
dur_ord (s0, e0) (s1, e1) = compare (e0 - s0) (e1 - s1)

-- | Generate score from waveset data.
mk_score :: Double -> [Double] -> [(Double, Double)] -> [OSC]
mk_score sr repeats w =
    let durations = zipWith (\(s, e) r -> (e - s) * r / sr) w repeats
        start_times = scanl (+) 0 durations
        mk_elem (s,e) t d = Bundle (UTCr t) [mk_msg 10 s e d]
    in zipWith3 mk_elem w start_times durations

-- | n randomly chosen elements of w.
rchoose :: Int -> [a] -> [a]
rchoose n w =
    let (l, r) = (1, length w)
        a = listArray (l,r) w
    in take n (map (a !) (randomRs (l,r) (mkStdGen 1)))

-- | Load waveset instrument, allocate sound file buffer, do waveset
--   analysis, generate & play scores.
run_waveset :: Transport t => t -> String -> IO ()
run_waveset fd fn = do
  async fd (d_recv (synthdef "waveset" waveset))
  async fd (b_allocRead 10 fn 0 0)
  (nc, nf, sr) <- sf_info fn
  b <- sf_channel fn 0
  let w = ws (prune 64 0 (fzc 0 b))
      pl s = play_score 10 fd s >> pauseThread 1
  putStrLn ("#f: " ++ show (nc, nf, sr))
  putStrLn ("#w: " ++ show (length w)) -- force w
  pl (mk_score sr (repeat 1) w)
  pl (mk_score sr (repeat 2) (reverse w))
  pl (mk_score sr (cycle [2,4,8]) (sortBy dur_ord w))
  pl (mk_score sr (randomRs (1,24) (mkStdGen 2)) (rchoose 512 w))

main :: IO ()
main = do
  (fn:_) <- getArgs
  withSC3 (\fd -> run_waveset fd fn)

{--
withSC3 (\fd -> run_waveset fd "/home/rohan/audio/text.snd")
--}
