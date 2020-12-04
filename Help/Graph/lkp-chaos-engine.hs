-- http://sccode.org/1-5aJ -- chaos engine hum
let o1 = sinOsc KR 101 (saw KR 0.12345 * 678 + 9) * 0.2 + 0.8
    o2 = pulse KR (mce2 25 25.5) 0.25 * 0.125 - 0.25
    o3 = sinOsc KR (mce2 50 51) 0 * o1 + o2
in sinOsc AR (10 + 50 * o3) 0 * 0.5
