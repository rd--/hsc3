-- http://sccode.org/1-5aJ -- chaos engine hum
let o1 = sinOsc kr 101 (saw kr 0.12345 * 678 + 9) * 0.2 + 0.8
    o2 = pulse kr (mce2 25 25.5) 0.25 * 0.125 - 0.25
    o3 = sinOsc kr (mce2 50 51) 0 * o1 + o2
in sinOsc ar (10 + 50 * o3) 0 * 0.5
