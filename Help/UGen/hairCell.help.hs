-- hairCell ; constantly self oscillates at 5 Hz
pan2 (X.hairCell (soundIn 0) 5.0 100 1000 0.99) 0 0.1

-- hairCell
pan2 (X.hairCell (saw AR (mouseX KR 1 10 Linear 0.2)) 0 (mouseY KR 0 10000 Linear 0.2) 1000 0.99) 0 0.5
