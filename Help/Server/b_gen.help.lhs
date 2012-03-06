/b_gen                              Call a command to fill a buffer

int    - buffer number
string - command name
...    - command arguments

Plug-ins can define commands that operate on buffers. The arguments
after the command name are defined by the command. Replies to sender
with /done when complete.

## Wave Fill Commands

The flags are enumerated at 'B_Gen'.

  Normalize - Normalize peak amplitude of wave to 1.0.

  Wavetable - If set, then the buffer is written in wavetable format
so that it can be read by interpolating oscillators.

  Clear - if set then the buffer is cleared before new partials are
written into it. Otherwise the new partials are summed with the
existing contents of the buffer.

The commands are at 'b_gen_sine1', 'b_gen_sine2', 'b_gen_sine3' and
'b_gen_cheby'.

sine1 [partial-amplitude]

Fills a buffer with a series of sine wave partials. The first float
value specifies the amplitude of the first partial, the second float
value specifies the amplitude of the second partial, and so on.

sine2 [(partial-frequency,partial-amplitude)]

Similar to sine1 except that each partial frequency is specified
explicitly, in cycles per buffer, instead of being an integer series
of partials. Non-integer partial frequencies are possible.

sine3 [(partial-frequency,partial-amplitude,partial-phase)]

Similar to sine2 except that each partial may have a nonzero starting
phase.

cheby [(amplitude)]

Fills a buffer with a series of chebyshev polynomials, which can be
defined as:

    cheby(n) = amplitude  * cos(n * acos(x))

The first float value specifies the amplitude for n = 1, the second
float value specifies the amplitude for n = 2, and so on. To eliminate
a DC offset when used as a waveshaper, the wavetable is offset so that
the center value is zero.

## Other Commands

copy

    int         sample position in destination
    int         source buffer number
    int         sample position in source
    int         number of samples to copy

Copy samples from the source buffer to the destination buffer
specified in the b_gen command. If the number of samples to copy is
negative, the maximum number of samples possible is copied.

Asynchronous.
