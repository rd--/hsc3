-- pv_MagBelow ; c.f. pv_MagAbove
let z = soundIn 0
    f = fft' (localBuf 'α' 2048 1) z
    x = mouseX kr 0 64 Linear 0.1
    h = pv_MagBelow f x
in ifft' h * 0.5

-- pv_MagBelow ; synthesised input
let z = sinOsc ar (sinOsc kr (squared (sinOsc kr 0.08 0 * 6 + 6.2)) 0 * 100 + 800) 0
    f = fft' (localBuf 'α' 2048 1) z
    x = mouseX kr 0 512 Linear 0.1
    h = pv_MagBelow f x
in ifft' h * 0.1
