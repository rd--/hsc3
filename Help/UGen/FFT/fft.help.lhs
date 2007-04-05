fft buffer in

Fast fourier transform.  The fast fourier transform analyzes the
frequency content of a signal.  fft uses a local buffer for holding
the buffered audio.  The inverse transform, ifft, reconstructs an
audio signal.

The fft and pv_* UGens run at control rate, the ifft UGen at audio
rate.

> withSC3 (\fd -> do send fd (b_alloc 10 2048 1)
>                    wait fd "/done")
> n <- whiteNoise AR
> audition (out 0 (ifft (fft 10 (n * 0.05))))

> withSC3 (\fd -> do send fd (b_alloc 10 2048 1)
>                    wait fd "/done")
> let s0 = sinOsc KR 0.08 0 * 6 + 6.2
>     s1 = sinOsc KR (squared s0) 0 * 100 + 800
>     s2 = sinOsc AR s1 0
> audition (out 0 (ifft (fft 10 s2) * 0.25))
