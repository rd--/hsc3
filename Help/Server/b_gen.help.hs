---- ; function to generate wavetable buffer using b_gen_cheby
mk_cheby a =
  let tbl_f = [Normalise, Wavetable, Clear]
      msg = [b_alloc 10 512 1, b_gen_cheby 10 tbl_f a]
  in withSC3 (mapM_ async msg)

---- ; generate various wavetables
mk_cheby [1, 0, 1, 1, 0, 1]
mk_cheby [0.25, 0.5, 0.25]
mk_cheby [1, 0, 1, 1, 0, 1, 0.5, 0, 0.25, 0, 0.75, 1]

---- ; generate sin table ; not wavetable
withSC3 (mapM_ maybe_async [b_alloc 10 256 1,b_gen_sine1 10 [Normalise, Clear] [1, 1/2, 1/3, 1/4, 1/5]])

---- ; plot wavetable (as in-buffer layout, as plain wavetable)
import Sound.SC3.Plot {- hsc3-plot -}
withSC3 (Sound.SC3.Plot.plot_buffer1 10)
withSC3 (Sound.SC3.Plot.plot_wavetable1 10)

---- ; fetch generated buffer data ; compare to haskell generated buffer
withSC3 (b_fetch1 512 10)
Gen.sine1_nrm 256 [1, 1/2, 1/3, 1/4, 1/5]

---- ; generate tables in haskell
Sound.SC3.Plot.plot_p1_ln [Gen.cheby 256 [1, 0, 1, 1, 0, 1]]
Sound.SC3.Plot.plot_p1_ln [Gen.cheby 256 [0.25, 0.5, 0.25]]
Sound.SC3.Plot.plot_p1_ln [Gen.cheby 256 [1, 0, 1, 1, 0, 1, 0.5, 0, 0.25, 0, 0.75, 1]]
Sound.SC3.Plot.plot_p1_ln [Gen.chebyShaperTbl 256 [1, 0, 1, 1, 0, 1, 0.5, 0, 0.25, 0, 0.75, 1]]
Sound.SC3.Plot.plot_p1_ln [Gen.sine1_nrm 256 [1, 1/2, 1/3, 1/4, 1/5]]

----- ; open remote scdoc help
Sound.SC3.Common.Help.sc3_scdoc_help_server_command_open False "/b_gen"
