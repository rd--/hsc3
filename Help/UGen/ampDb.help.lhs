> import Sound.SC3 {- hsc3 -}

    > let amp = map (2 **) [0 .. 15]
    > let db = [0,-6 .. -90]
    > map (round . ampDb . (/) 1) amp == db
    > map (round . amp_to_db . (/) 1) amp == db
    > zip amp db

    > db_to_amp (-3) == 0.7079457843841379
    > amp_to_db 0.7079457843841379 == -3
