---- ;help
Sound.Sc3.sc3_scdoc_help_server_command_open False "/b_allocReadChannel"

> import Data.List {- base -}
> import Data.List.Split {- split -}
> import Sound.Sc3 {- hsc3 -}

Read an audio file with many channels (here 1024)

> fn_00 = "/home/rohan/sw/hsc3-sf/data/au/mc-10-1024.au"

> m_00 = b_allocRead 0 fn_00 0 0

    > withSc3 (async_ m_00)

Query buffer

    > withSc3 (b_query1_unpack 0)

Read only specified channels

> m_01 = b_allocReadChannel 0 fn_00 0 0 [0 .. 7]

    > withSc3 (async_ m_01)

Query buffer

    > withSc3 (b_query1_unpack 0)

Examine buffer

    > let (nf,nc) = (10,8)
    > d <- withSc3 (b_getn1_data_segment 128 0 (0,nf * nc))
    > (putStrLn . unlines . transpose . chunksOf nc . map (\n -> if n > 0 then '1' else '0')) d
