-- scratchy (jmcc) #1
let n = mce (map (\z -> brownNoise z AR * 0.5 - 0.49) (id_seq 2 'α'))
in rhpf (max n 0 * 20) 5000 1
