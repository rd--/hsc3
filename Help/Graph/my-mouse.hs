-- https://mohayonao.github.io/CoffeeCollider/#mouse.coffee (mohayonao) http://the.mohayonao.com/
let freq = mouseY KR 100 1000 Exponential 0.1
    freq1 = freq * mouseX KR 2 0.5 Linear 2.5
    freq2 = freq * mouseX KR 0.5 2 Linear 2.5
    feedback = mouseButton KR 0 pi 5
in sinOscFB AR (mce2 freq1 freq2) feedback * 0.1
