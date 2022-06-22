-- streson ; http://www.csounds.com/manual/html/streson.html
let z = lfSaw ar (mce2 220 180) 0 * 0.2
    dt = recip (linExp (lfCub kr 0.1 (0.5 * pi)) (-1) 1 280 377)
in X.streson z dt 0.9 * 0.1

-- streson ; c.f. Sound.Sc3.Data.Modal.modal_frequency_ratios
let dt = recip (linExp (lfCub kr 0.1 (0.5 * pi)) (-1) 1 280 377)
in X.streson (soundIn 0) dt 0.9 * 0.1

---- ; drawings
Sound.Sc3.Plot.plot_ugen_nrt (20,1) 16.0 (recip (linExp (lfCub kr 0.1 (0.5 * pi)) (-1) 1 280 377))
