import Sound.OpenSoundControl
import Sound.SC3
import System.Cmd

graph :: UGen
graph =
    let f = control KR "freq" 440
        g = control KR "gain" 0.1
        e = envGen KR 1 1 0 1 RemoveSynth (envPerc 0.1 1.0)
        o = sinOsc AR f 0 * e
    in out 0 (pan2 o 0 g)

score :: [OSC]
score =
    let at t m = Bundle (NTPr t) m
        instr = d_recv (synthdef "test" graph)
        group = g_new [(1, AddToTail, 0)]
        node t f = at t [s_new "test" (-1) AddToTail 1 [("freq", f)]]
        notes = take 128 (zipWith node [0.0, 0.05 ..] [330, 350 ..])
    in at 0.0 [instr, group] : notes

main :: IO ()
main = do
  writeNRT "/tmp/nrt.score" score
  system "scsynth -N /tmp/nrt.score _ /tmp/nrt.wav 44100 WAVE float"
  return ()
