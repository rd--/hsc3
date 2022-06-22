-- | Windowing functions.
module Sound.Sc3.Common.Math.Window where

-- * Type and conversion

-- | A function from a (0, 1) normalised input to an output.
type Window x = x -> x

-- | A discrete rendering of a 'Window'.
type Table x = [x]

{- | Format for table.
     Closed indicates the end point should be equal to the start point.
     Open indicates it should be one place short.
     Guarded indicates that an extra place should be added that closes the table, ie. the table has one place more than requested.
     When using a table with an oscillator we want an Open or Guarded table, since the point following the end point is the start point.
-}
data TableFormat = TableClosed | TableOpen | TableGuarded deriving (Eq, Show)

{- | Generate an /n/ element table from a (0, 1) normalised window function /f/.
     The cycle argument decides if the end point should be equal to the start point, or one place short.
     When using a table with an oscillator we want the latter, since the point following the end point is the start point.
-}
window_table :: (Integral n,Fractional a,Enum a) => TableFormat -> n -> Window a -> Table a
window_table fmt n f =
  let k = 1 / (fromIntegral n - (if fmt == TableClosed then 1 else 0))
  in take (fromIntegral (if fmt == TableGuarded then n + 1 else n)) (map f [0, k ..])

-- | window_table of TableClosed.
window_table_closed :: (Integral n,Fractional a,Enum a) => n -> Window a -> Table a
window_table_closed = window_table TableClosed

-- * Math

-- | /n/ ^ 2.
square :: Num a => a -> a
square x = x * x

-- * Window functions

-- | Gaussian window, θ <= 0.5.
gaussian :: Floating a => a -> Window a
gaussian theta i = exp (- (0.5 * square ((i - 0.5) / (theta * 0.5))))

-- | Hann raised cosine window.
hann :: Floating a => Window a
hann i = 0.5 * (1 - cos (2 * pi * i))

-- | Hamming raised cosine window.
hamming :: Floating a => Window a
hamming i = 0.54 - 0.46 * cos (2 * pi * i)

-- | Unit ('id') window, also known as a Dirichlet window.
rectangular :: Window a
rectangular = id

-- | 'sin' window.
sine :: Floating a => Window a
sine i = sin (i * pi)

{- | Triangular window, ie. Bartlett window with zero end-points.

> let n = 2 ^ 7
> Sound.Sc3.Plot.plot_p1_ln (map (\fmt -> window_table fmt n triangular) [TableClosed, TableOpen])
> Sound.Sc3.Plot.plot_p1_ln (map (\fmt -> window_table fmt n triangular) [TableClosed, TableGuarded])
-}
triangular :: Fractional a => Window a
triangular i = 2 * (0.5 - abs (i - 0.5))

-- * Tables

{- | 'window_table_closed' . 'gaussian'.

> Sound.Sc3.Plot.plot_p1_ln [gaussian_table 1024 0.25, gaussian_table 1024 0.5]
-}
gaussian_table :: (Integral n, Floating b, Enum b) => n -> b -> [b]
gaussian_table n = window_table_closed n . gaussian

{- | 'window_table_closed' . 'hamming'.

> Sound.Sc3.Plot.plot_p1_ln [hann_table 128, hamming_table 128]
-}
hamming_table :: Int -> [Double]
hamming_table n = window_table_closed n hamming

{- | 'window_table_closed' . 'hann'.

> Sound.Sc3.Plot.plot_p1_ln [hann_table (2 ^ 7)]
-}
hann_table :: Int -> [Double]
hann_table n = window_table_closed n hann

{- | 'window_table_closed' . 'sine'.

> Sound.Sc3.Plot.plot_p1_ln [sine_table (2 ^ 7)]
-}
sine_table :: (Integral n, Floating b, Enum b) => n -> [b]
sine_table n = window_table_closed n sine

{- | 'window_table_closed' . 'triangular'.

> Sound.Sc3.Plot.plot_p1_ln [triangular_table (2 ^ 8)]
-}
triangular_table :: (Integral n, Fractional b, Enum b) => n -> [b]
triangular_table n = window_table_closed n triangular
