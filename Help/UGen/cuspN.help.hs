-- cuspN ; vary frequency
let x = mouseX KR 20 (sampleRate / 2) Linear 0.1
in cuspN AR x 1.0 1.99 0 * 0.1
