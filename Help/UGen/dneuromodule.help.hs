-- dneuromodule
let dt = 0.001
    theta = mce2 (mouseX kr (-2) 2 Linear 0.2) (mouseY kr (-2) 2 Linear 0.2)
    md = X.dneuromoduleId 2 'α' 2 theta (mce2 0.1 0.3) (mce [-3,2,-2,0])
    m = lag3 (duty ar dt 0 DoNothing md) dt
in sinOsc ar (m * 400 + 600) 0 * 0.05
