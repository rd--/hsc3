> Sound.SC3.Server.Help.viewServerHelp "/b_allocReadChannel"

> import Sound.SC3

Read an audio file with many channels (here 2000)

> let fn = "/home/rohan/sw/hsc3-sf/au/mc-12-2000.au"

> withSC3 (async (b_allocRead 0 fn 0 0))

Query buffer

> withSC3 (b_query1_unpack 0)

Read only specified channels

> withSC3 (async (b_allocReadChannel 0 fn 0 0 [0..1023]))

Query buffer

> withSC3 (b_query1_unpack 0)

Examine buffer

> import Data.List {- base -}
> import Data.List.Split {- split -}
> let (nf,nc) = (12,1024)
> d <- withSC3 (b_getn1_data_segment 128 0 (0,nf * nc))
> (putStrLn . unlines . transpose . chunksOf nc . map (\n -> if n > 0 then '1' else '0')) d
