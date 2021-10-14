(
var wr = {arg fp, b; var d = b.asRawOSC; fp.write(d.size); fp.write(d)};
var sy = SynthDef("sin",{arg freq = 440; Out.ar(0, SinOsc.ar(freq, 0) * 0.2)});
var b0 = [0.0, [\g_new, 1, 1, 0], [\d_recv, sy.asBytes]];
var b1 = [0.5, [\s_new, \sin, 1001, 0, 1]];
var b2 = [3.0, [\n_free, 1001]];
var b3 = [3.5, [\nrt_end]];
File.use("/tmp/nrt.01.osc", "w", {arg fp; [b0,b1,b2,b3].do{arg i; wr.(fp,i)}});
)

"scsynth -i 0 -o 1 -N /tmp/nrt.01.osc _ /tmp/nrt.01.wav 48000 WAV int16".unixCmd;
