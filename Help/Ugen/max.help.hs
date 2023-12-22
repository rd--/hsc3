-- max
(sinOsc ar 500 0 `max` sinOsc ar 0.1 0) * 0.1

-- max ; rate selection ; derive from left
let f = (sinOsc ar 0.2 0 `max` sinOsc kr 0.9 0) * 200 + 300
in sinOsc ar f 0 * 0.1

-- max ; rate selection ; derive from right
let f = (sinOsc kr 0.2 0 `max` sinOsc ar 0.9 0) * 200 + 300
in sinOsc ar f 0 * 0.1
