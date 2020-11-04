-- freeSelf
let n = dust 'α' KR 0.5
in mrg [sinOsc AR 440 0 * 0.1,freeSelf n]
