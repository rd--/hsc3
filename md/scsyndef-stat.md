# scsyndef-stat

Read the binary representation of a
[SuperCollider](http://audiosynth.com) graph and print the output of
the [hsc3](?t=hsc3) function `graph_stat`.

~~~~
$ hsc3-scsyndef-stat ~/sw/hsc3-graphs/scsyndef/why_supercollider.scsyndef
number of constants       : 12
number of controls        : 0
control rates             : []
number of unit generators : 189
unit generator rates      : [(IR,37),(KR,21),(AR,131)]
~~~~

The last entry lists the `UGen`s in sequence on one line, which can be
very long.  Here, it is written as a paragraph:

~~~~
unit generator sequence   : Dust,*,Rand,Resonz,Dust,*,Rand,Resonz,Dust,*,
Rand,Resonz,Dust,*,Rand,Resonz,Sum4,Dust,*,Rand,Resonz,Dust,*,Rand,Resonz,
Dust,*,Rand,Resonz,Sum4,Dust,*,Rand,Resonz,Dust,*,Rand,Resonz,Dust,*,Rand,
Resonz,Sum4,Dust,*,Rand,Resonz,Dust,*,Rand,Resonz,Dust,*,Rand,Resonz,Dust,
*,Rand,Resonz,Sum4,Dust,*,Rand,Resonz,Dust,*,Rand,Resonz,Dust,*,Rand,
Resonz,Sum4,Dust,*,Rand,Resonz,Dust,*,Rand,Resonz,Dust,*,Rand,Resonz,Sum4,
DelayN,Rand,LFNoise1,*,+,CombL,Sum4,Sum4,Dust,*,Rand,Resonz,Sum4,DelayN,Rand,
LFNoise1,*,+,CombL,Sum4,Sum4,Dust,*,Rand,Resonz,Sum4,DelayN,Rand,LFNoise1,*,+,
CombL,Sum4,Sum4,Dust,*,Rand,Resonz,Sum4,DelayN,Rand,LFNoise1,*,+,CombL,Sum4,
Sum4,Sum4,Dust,*,Rand,Resonz,Sum4,DelayN,Rand,LFNoise1,*,+,CombL,Sum4,Sum4,
Dust,*,Rand,Resonz,Sum4,DelayN,Rand,LFNoise1,*,+,CombL,Sum4,Sum4,Dust,*,Rand,
Resonz,Sum4,DelayN,Rand,LFNoise1,*,+,CombL,Sum4,RandN,AllpassN,RandN,AllpassN,
RandN,AllpassN,RandN,AllpassN,*,+,AllpassN,AllpassN,AllpassN,AllpassN,*,+,Out
$
~~~~
