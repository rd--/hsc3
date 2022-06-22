-- rSmplrIndex ; event control ; stereo ; requires=b0 ; loaded bosendorfer (or equivalent) sample data
let f (_,g,x,y,z,_,_,_,_,_,_) =
      let s_mnn = [21,23,24,26,28,29,31,33,35,36,38,40,41,43,45,47,48,50,52,53,55,57,59
                  ,60,62,64,65,67,69,71,72,74,76,77,79,81,83,84,86,88,89,91,93,95,96,98
                  ,100,101,103,105,107,108]
          tbl = asLocalBufId 'α' s_mnn
          mnn = x * 88 + 21
          mnn0 = latch mnn g
          rt = midiRatio ((mnn - mnn0) * y)
          (buf,rt0) = unmce2 (X.rSmplrIndex kr tbl (bufFrames kr tbl) mnn0)
          b0 = control kr "b0" 100
          sig = playBuf 2 ar (b0 + buf) (bufRateScale kr (b0 + buf) * rt0 * rt) g 0 NoLoop DoNothing
      in sig * z * lagUD g 0 4
in mix (voicer 16 f) * control kr "gain" 1

-- rSmplrIndex ; event control ; stereo ; requires=tbl,b0 ; data table and loaded sample data
let f (_,g,x,y,z,_,_,_,_,_,_) =
      let tbl = control kr "tbl" 0
          lhs = bufRd 1 kr tbl 0 NoLoop NoInterpolation
          rhs = bufRd 1 kr tbl (bufFrames kr tbl - 1) NoLoop NoInterpolation
          mnn = x * (rhs - lhs) + lhs
          mnn0 = latch mnn g
          rt = midiRatio ((mnn - mnn0) * y)
          (buf,rt0) = unmce2 (X.rSmplrIndex kr tbl (bufFrames kr tbl) mnn0)
          b0 = control kr "b0" 100
          sig = playBuf 2 ar (b0 + buf) (bufRateScale kr (b0 + buf) * rt0 * rt) g 0 NoLoop DoNothing
      in sig * z * lagUD g 0 4
in mix (voicer 16 f) * control kr "gain" 1

-- rSmplrIndex ; event control ; mono ; requires=tbl,b0 data table and loaded sample data
let f (_,g,x,y,z,o,_,_,_,_,_) =
      let tbl = control kr "tbl" 0
          lhs = bufRd 1 kr tbl 0 NoLoop NoInterpolation
          rhs = bufRd 1 kr tbl (bufFrames kr tbl - 1) NoLoop NoInterpolation
          mnn = x * (rhs - lhs) + lhs
          mnn0 = latch mnn g
          rt = midiRatio ((mnn - mnn0) * y)
          (buf,rt0) = unmce2 (X.rSmplrIndex kr tbl (bufFrames kr tbl) mnn0)
          b0 = control kr "b0" 100
          sig = playBuf 1 ar (b0 + buf) (bufRateScale kr (b0 + buf) * rt0 * rt) g 0 NoLoop DoNothing
      in pan2 sig (o * 2 - 1) (z * lagUD g 0.01 0.01)
in mix (voicer 16 f) * control kr "gain" 8

---- ; load bosdendorfer sample data
hsc3-smplr load pf --dyn=008 --b0=100

---- ; load SFZ sample data
import Sound.OSC {- hosc -}
import Sound.Sc3.Data.SFZ {- hsc3-data -}
sfz_fn = sfResolve "instr/bosendorfer/008.sfz"
sfz_fn = sfResolve "instr/farfisa/aad/dolce-8.sfz"
sfz_fn = sfResolve "instr/casacota/zell_1737_415_MeanTone5/8_i.sfz"
(ctl,glb,rgn) <- sfz_load_data sfz_fn
tbl = map ((\(k,_,_) -> k) . sfz_region_key) rgn
withSc3 (sendMessage (b_alloc_setn1 0 0 (map fromIntegral tbl)))
alloc_msg = zipWith (\b fn -> b_allocRead b fn 0 0) [100..] (map (sfz_region_sample_resolve sfz_fn ctl) rgn)
withSc3 (mapM_ async alloc_msg)
