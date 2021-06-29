-- switchDelay ; simple feedback delay
X.switchDelay (soundIn 0) 1 1 1 0.99 20

-- switchDelay ; simple feedback delay
X.switchDelay (soundIn 0) 1 0.7 0.4 0.6 20

-- switchDelay ; change the buffer read pointer periodically.
let ix = stepper (impulse kr 0.5 0) 0 0 3 1 0
    dt = select ix (mce [0.02,0.1,0.725,0.25])
in X.switchDelay (soundIn 0) 1 0.6 dt 0.7 20
