-- rdl
X.rdl ar 2 (mce2 0 0)

---- ; notes

rdl is a host for [hdf](?t=hdf) generated DSP codes.

rdl codes can read buffers and control signals.  Required resources
must be correctly pre-allocated.

rdl nodes are instantiated with a fixed number of channels, it is an
error to load incompatible codes.

Codes are loaded using u_cmd, which requires the ugenIndex of the
rdl node.

> import Sound.Sc3.UGen.Graph {- hsc3 -}
> ug_ugen_indices "RDL" (ugen_to_graph (X.rdl 2 (mce2 0 0))) == [0]

When created, the node has no code loaded.

hdf includes a u_cmd_g_load function.  Codes can be replaced while
the rdl node is running.  The graphs below are given as examples at
hdf.

> withSc3 (sendMessage (DF.u_cmd_g_load (-1) 0 "/tmp/analog-bubbles.so"))
> withSc3 (sendMessage (DF.u_cmd_g_load (-1) 0 "/tmp/silence.so"))
> withSc3 (sendMessage (DF.u_cmd_g_load (-1) 0 "/tmp/moto-rev.so"))
> withSc3 (sendMessage (DF.u_cmd_g_load (-1) 0 "/tmp/sprinkler.so"))
> withSc3 (sendMessage (DF.u_cmd_g_load (-1) 0 "/tmp/lfo-modulation.so"))
