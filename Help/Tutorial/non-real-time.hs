import Sound.OpenSoundControl
import Sound.SC3
import System.Cmd

graph :: UGen
graph = out 0 (pan2 o 0 g)
    where f = control KR "freq" 440
          g = control KR "gain" 0.1
          e = envGen KR 1 1 0 1 RemoveSynth (envPerc 0.1 1.0)
          o = sinOsc AR f 0 * e

score :: [OSC]
score = at 0.0 [mk_instr, mk_group] : notes
    where at t m = Bundle (NTPr t) m
          mk_instr = d_recv (synthdef "test" graph)
          mk_group = g_new [(1, AddToTail, 0)]
          mk_node t f = at t [s_new "test" (-1) AddToTail 1 [("freq", f)]]
          notes = take 128 (zipWith mk_node [0.0, 0.05 ..] [330, 350 ..])

main :: IO ()
main = do writeNRT "/tmp/nrt.score" score
          system "scsynth -N /tmp/nrt.score _ /tmp/nrt.wav 44100 WAVE float"
          return ()
