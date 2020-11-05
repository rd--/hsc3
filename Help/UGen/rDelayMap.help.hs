-- rDelayMap
let a = mce [-1,0,0,0.5,1,-1,0,1]
    b = clearBuf (localBuf 'α' 1 88200)
    x = mouseX KR 110 440 Linear 0.1
    y = mouseY KR 0.0 0.2 Linear 0.1
    o = sinOsc AR x 0 * y
in mce [X.rDelayMap b o 0 a,o]

-- rDelayMap ; simple feedback circuit (static)
let n = whiteNoise 'α' AR
    b = clearBuf (localBuf 'β' 1 88200)
    s = decay (impulse AR (5/4) 0) 0.1 * n * 0.2
    a = mce [-1,0,0,1,1/9,0,1,8/9,1,-1,0,1]
in X.rDelayMap b s 0 a

-- rDelayMap ; simple feedback circuit (dynamic)
let n = whiteNoise 'α' AR -- soundIn 0
    s = decay (impulse AR (mce [1/3,5/4]) 0) 0.1 * n * 0.2
    x = mouseX KR 0.05 1.05 Linear 0.1
    y = mouseY KR 0.05 0.95 Linear 0.1
    a = mce [-1,0,0,1,y,0,1,x,1,-1,0,1]
    b = clearBuf (localBuf 'β' 1 88200)
in X.rDelayMap b s 1 a

---- ; Network of delay line maps

{- Create a network of delay line maps.  A map is defined by a
quadruple: source location, destination location, operation and
gain.  The locations are specified in seconds, a negative location
specifies the 'input' location for sources and the 'output'
location for destinations.  The operation is specified as an
integer, zero is move, one is add, two is subtract, three is
multiply, four is divide.  The gain is linear.  This UGen is
useful for implementing nested filters, as described by William
Gardner in his MS thesis "The Virtual Acoustic Room", MIT 1992. -}
