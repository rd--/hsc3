lfCub freq iphase
 
A sine like shape made of two cubic pieces. Smoother than lfPar.

> audition $ lfCub AR (lfCub KR (lfCub KR 0.2 0 * 8 + 10) 0 * 400 + 800) 0 * 0.1
> audition $ lfCub AR (lfCub KR 0.2 0 * 400 + 800) 0 * 0.1
> audition $ lfCub AR 800 0 * 0.1
> audition $ lfCub AR (xLine KR 100 8000 30 DoNothing) 0 * 0.1

Compare (lfPar):

> audition $ lfPar AR (lfPar KR (lfPar KR 0.2 0 * 8 + 10) 0 * 400 + 800) 0 * 0.1
> audition $ lfPar AR (lfPar KR 0.2 0 * 400 + 800) 0 * 0.1
> audition $ lfPar AR 800 0 * 0.1
> audition $ lfPar AR (xLine KR 100 8000 30 DoNothing) 0 * 0.1

Compare (sinOsc):

> audition $ sinOsc AR (sinOsc KR (sinOsc KR 0.2 0 * 8 + 10) 0 * 400 + 800) 0 * 0.1
> audition $ sinOsc AR (sinOsc KR 0.2 0 * 400 + 800) 0 * 0.1
> audition $ sinOsc AR 800 0 * 0.1
> audition $ sinOsc AR (xLine KR 100 8000 30 DoNothing) 0 * 0.1

Compare (lfTri):

> audition $ lfTri AR (lfTri KR (lfTri KR 0.2 0 * 8 + 10) 0 * 400 + 800) 0 * 0.1
> audition $ lfTri AR (lfTri KR 0.2 0 * 400 + 800) 0 * 0.1
> audition $ lfTri AR 800 0 * 0.1
> audition $ lfTri AR (xLine KR 100 8000 30 DoNothing) 0 * 0.1
