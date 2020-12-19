-- dneuromodule
let dt = 0.001
    theta = mce2 (mouseX KR (-2) 2 Linear 0.2) (mouseY KR (-2) 2 Linear 0.2)
    md = X.dneuromodule 2 'α' 2 theta (mce2 0.1 0.3) (mce [-3,2,-2,0])
    m = lag3 (duty AR dt 0 DoNothing md) dt
in sinOsc AR (m * 400 + 600) 0 * 0.05
