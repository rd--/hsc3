# hsc3-help

## sc3-help rtf

Locates the RTF file for a subject,
runs [unrtf](https://www.gnu.org/software/unrtf/) to write a `.scd` file,
which it opens in [emacs](https://www.gnu.org/software/emacs/).
The RTF files must be at `SC3_RTF_HELP_DIR`.

~~~~
$ echo $SC3_RTF_HELP_DIR
/home/rohan/opt/src/sc3-help/Help
$ hsc3-help sc3-help rtf SinOsc lfSaw softclip
$
~~~~

## sc3-help scdoc-local

Locates entries for some subjects at the
SC3-DOC HTML system and opens them using `BROWSER` or `x-www-browser`.

~~~~
$ echo $SC3_SCDOC_HTML_HELP_DIR
/home/rohan/.local/share/SuperCollider/Help
$ hsc3-help sc3-help scdoc-local SinOsc Collection.inject 'Collection.*fill'
$
~~~~

## ugen-control-param

Prints the default values of UGen parameters in sequence as named control values.

~~~~
$ hsc3-help ugen-control-param pitch
Pitch KR (control KR "in" 0.0) (control KR "initFreq" 440.0) ...
$
~~~~

## ugen-default-param

Prints the default values of UGen parameters in sequence.

~~~~
$ hsc3-help ugen-default-param sinOsc # oscillator
AR 440.0 0.0
$ hsc3-help ugen-default-param resonz # filter
0.0 440.0 1.0
$ hsc3-help ugen-default-param pitch # fixed rate
0.0 440.0 60.0 4000.0 100.0 16.0 1.0 0.01 0.5 1.0 0.0
$
~~~~

## ugen-summary

Prints UGen information from [hsc3-db](?t=hsc3-db).

~~~~
$ hsc3-help ugen-summary bufRd demand
Buffer reading oscillator.

BufRd [KR,AR] bufnum=0.0 phase=0.0 loop=1.0 interpolation=2.0
    NC INPUT: True, ENUMERATION INPUTS: 2=Loop, 3=Interpolation

Demand results from demand rate UGens.

Demand [KR,AR] trig=0.0 reset=0.0 *demandUGens=0.0
    MCE=1, FILTER: TRUE
~~~~
