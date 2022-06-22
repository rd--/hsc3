-- decay ; ordinary graph ; ordinary key bindings place at head of group one
let d = dustId 'α' ar 1
    n = whiteNoiseId 'β' ar
in decay (d * 0.5) 0.2 * n

---- ; signal/effect model using separate group, operating at the same bus
withSc3 (playAt (-1, AddToTail, 2, []) (replaceOut 0 (combC (in' 1 ar 0) 0.2 0.2 3)))
