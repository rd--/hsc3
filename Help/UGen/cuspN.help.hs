-- cuspN ; vary frequency
let x = mouseX kr 20 (sampleRate / 2) Linear 0.1
in cuspN ar x 1.0 1.99 0 * 0.1
