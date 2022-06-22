-- freeSelf
let n = dustId 'α' kr 0.5
in mrg [sinOsc ar 440 0 * 0.1,freeSelf n]
