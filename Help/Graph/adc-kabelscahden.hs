-- https://swiki.hfbk-hamburg.de/MusicTechnology/851 march 2003 (adc) / (jrhb)
let freq = 50
    sustain = 1
    amp = 0.5
    hum = let t = dust 'α' KR (7 ** lfNoise1 'β' KR 0.3)
              h = toggleFF (coinGate 'γ' 0.4 t) * rlpf (lfPulse AR freq 0 0.5 + (lfNoise1 'δ' KR 2 * 0.5 - 0.5)) 6000 0.15
              n = trig t (tRand 'ε' 0 0.01 (coinGate 'ζ' 0.4 t)) * whiteNoise 'η' AR
              m = trig t (tRand 'θ' 0 0.01 (coinGate 'ι' 0.4 t)) * brownNoise 'κ' AR
              k = trig t (lfNoise1 'λ' KR (mce2 4 4.2) * 0.1 + 0.11) * lfClipNoise 'μ' AR (lfNoise0 'ν' KR 7 * 30 + 40)
          in distort (leakDC ((h + n + m + k) * 10) 0.995)
    e = envGen KR 1 1 0 1 RemoveSynth (Envelope [amp,amp,0] [sustain,0] [] Nothing Nothing 0)
in out 0 (clip2 hum 1 * e * 0.25)
