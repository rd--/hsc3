-- sinOscFB
let x = mouseX kr 0 4 Linear 0.2
in sinOscFB ar (mce2 400 301) x * 0.1

-- sinOscFB
let y = mouseY kr 10 1000 Exponential 0.2
    x = mouseX kr (pi/2) pi Linear 0.2
in sinOscFB ar y x * 0.1

-- sinOscFB
let y = mouseY kr 1 1000 Exponential 0.2
    x = mouseX kr (pi/2) pi Linear 0.2
in sinOscFB ar (100 * sinOscFB ar y 0 + 200) x * 0.1
