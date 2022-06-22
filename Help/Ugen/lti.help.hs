-- lti
let a = [0.02,-0.01]
    b = [1,0.7,0,0,0,0,-0.8,0,0,0,0,0.9,0,0,0,-0.5,0,0,0,0,0,0,0.25,0.1,0.25]
    z = pinkNoiseId 'α' ar * 0.1
in X.lti ar z (asLocalBufId 'β' a) (asLocalBufId 'γ' b)
