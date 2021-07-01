-- vbJonVerb ; default param
X.vbJonVerb (dustId 'α' ar 2) 0.7 0.3 0.8 0.5 0.5

-- vbJonVerb
let freq = mce (map (* 1000) [1,2.1,2.9])
    src = mix (resonz (mixFill 3 (\z -> dustId (z::Int) ar 0.25)) freq 0.01) * 10
in X.vbJonVerb src 0.8 0.3 0.8 0.1 0.5 + src
