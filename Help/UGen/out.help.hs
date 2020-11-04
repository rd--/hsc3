-- out ; oscillators at outputs zero (330hz) and one (331hz)
out 0 (sinOsc AR (mce2 330 331) 0 * 0.1)

-- out ; an out node is implicitly added by various hsc3 functions if not given
sinOsc AR (mce2 330 331) 0 * 0.1

-- out ; out is a summing output, see replaceOut for over-writing output
mrg [out 0 (sinOsc AR (mce2 330 990) 0 * 0.05)
    ,out 0 (sinOsc AR (mce2 331 991) 0 * 0.05)]
