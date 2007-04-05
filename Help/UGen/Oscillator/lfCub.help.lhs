lfCub rate freq iphase
 
A sine like shape made of two cubic pieces. Smoother than lfPar.

> audition (out 0 (lfCub AR (lfCub KR (lfCub KR 0.2 0 * 8 + 10) 0 * 400 + 800) 0 * 0.1))
> audition (out 0 (lfCub AR (lfCub KR 0.2 0 * 400 + 800) 0 * 0.1))
> audition (out 0 (lfCub AR 800 0 * 0.1))
> audition (out 0 (lfCub AR (xLine KR 100 8000 30 DoNothing) 0 * 0.1))

Compare (lfPar):

> audition (out 0 (lfPar AR (lfPar KR (lfPar KR 0.2 0 * 8 + 10) 0 * 400 + 800) 0 * 0.1))
> audition (out 0 (lfPar AR (lfPar KR 0.2 0 * 400 + 800) 0 * 0.1))
> audition (out 0 (lfPar AR 800 0 * 0.1))
> audition (out 0 (lfPar AR (xLine KR 100 8000 30 DoNothing) 0 * 0.1))

Compare (sinOsc):

> audition (out 0 (sinOsc AR (sinOsc KR (sinOsc KR 0.2 0 * 8 + 10) 0 * 400 + 800) 0 * 0.1))
> audition (out 0 (sinOsc AR (sinOsc KR 0.2 0 * 400 + 800) 0 * 0.1))
> audition (out 0 (sinOsc AR 800 0 * 0.1))
> audition (out 0 (sinOsc AR (xLine KR 100 8000 30 DoNothing) 0 * 0.1))

Compare (lfTri):

> audition (out 0 (lfTri AR (lfTri KR (lfTri KR 0.2 0 * 8 + 10) 0 * 400 + 800) 0 * 0.1))
> audition (out 0 (lfTri AR (lfTri KR 0.2 0 * 400 + 800) 0 * 0.1))
> audition (out 0 (lfTri AR 800 0 * 0.1))
> audition (out 0 (lfTri AR (xLine KR 100 8000 30 DoNothing) 0 * 0.1))
