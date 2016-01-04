# scsyndef-to-hs (July, 2012)

A _disassembler_ for UGen graphs, it reads the binary representation
of a [SuperCollider](http://audiosynth.com) _instrument_ and prints a
_plain_ [hsc3](?t=hsc3) (haskell) notation of the unit-generator
graph.

The _notated_ haskell form of the [why
supercollider?](?t=hsc3-graphs&e=gr/why-supercollider.scd) graph is:

~~~~
why_supercollider :: UGen
why_supercollider =
    let r = resonz (dust 'α' AR 0.2 * 50) (rand 'β' 200 3200) 0.003
        s = mix (uclone 'γ' 10 r)
        z = delayN s 0.048 0.048
        c = combL z 0.1 (lfNoise1 'δ' KR (rand 'ε' 0 0.1) * 0.04 + 0.05) 15
        y = mix (uclone 'ζ' 7 c)
        f i = allpassN i 0.05 (randN 2 'η' 0 0.05) 1
        x = useq 'θ' 4 f y
    in out 0 (s + 0.2 * x)
~~~~

The transcript below prints the _reconstruction_ of the binary
encoding of this graph in _plain_ notation:

~~~~
$ hsc3-scsyndef-to-hs ~/sw/hsc3-graphs/scsyndef/why_supercollider.scsyndef
import Sound.SC3
import Sound.SC3.Common
import Sound.SC3.UGen.Plain
why_supercollider :: UGen
why_supercollider =
  let c_0 = constant (1.0::Sample)
      c_1 = constant (15.0::Sample)
      c_2 = constant (0.05000000074505806::Sample)
      c_3 = constant (0.03999999910593033::Sample)
      c_4 = constant (0.10000000149011612::Sample)
      c_5 = constant (0.04800000041723251::Sample)
      c_6 = constant (0.003000000026077032::Sample)
      c_7 = constant (3200.0::Sample)
      c_8 = constant (200.0::Sample)
      c_9 = constant (50.0::Sample)
      c_10 = constant (0.20000000298023224::Sample)
      c_11 = constant (0.0::Sample)
      u_12 = nondet "Dust" (UId 12) AR [c_10] 1
      u_13 = binop CS "*" AR u_12 c_9
      u_14 = nondet "Rand" (UId 14) IR [c_8,c_7] 1
      u_15 = nondet "Resonz" (UId 15) AR [u_13,u_14,c_6] 1
      u_16 = nondet "Dust" (UId 16) AR [c_10] 1
      u_17 = binop CS "*" AR u_16 c_9
      u_18 = nondet "Rand" (UId 18) IR [c_8,c_7] 1
      u_19 = nondet "Resonz" (UId 19) AR [u_17,u_18,c_6] 1
      u_20 = nondet "Dust" (UId 20) AR [c_10] 1
      u_21 = binop CS "*" AR u_20 c_9
      u_22 = nondet "Rand" (UId 22) IR [c_8,c_7] 1
      u_23 = nondet "Resonz" (UId 23) AR [u_21,u_22,c_6] 1
      u_24 = nondet "Dust" (UId 24) AR [c_10] 1
      u_25 = binop CS "*" AR u_24 c_9
      u_26 = nondet "Rand" (UId 26) IR [c_8,c_7] 1
      u_27 = nondet "Resonz" (UId 27) AR [u_25,u_26,c_6] 1
      u_28 = nondet "Sum4" (UId 28) AR [u_15,u_19,u_23,u_27] 1
      u_29 = nondet "Dust" (UId 29) AR [c_10] 1
      u_30 = binop CS "*" AR u_29 c_9
      u_31 = nondet "Rand" (UId 31) IR [c_8,c_7] 1
      u_32 = nondet "Resonz" (UId 32) AR [u_30,u_31,c_6] 1
      u_33 = nondet "Dust" (UId 33) AR [c_10] 1
      u_34 = binop CS "*" AR u_33 c_9
      u_35 = nondet "Rand" (UId 35) IR [c_8,c_7] 1
      u_36 = nondet "Resonz" (UId 36) AR [u_34,u_35,c_6] 1
      u_37 = nondet "Dust" (UId 37) AR [c_10] 1
      u_38 = binop CS "*" AR u_37 c_9
      u_39 = nondet "Rand" (UId 39) IR [c_8,c_7] 1
      u_40 = nondet "Resonz" (UId 40) AR [u_38,u_39,c_6] 1
      u_41 = nondet "Sum4" (UId 41) AR [u_28,u_32,u_36,u_40] 1
      u_42 = nondet "Dust" (UId 42) AR [c_10] 1
      u_43 = binop CS "*" AR u_42 c_9
      u_44 = nondet "Rand" (UId 44) IR [c_8,c_7] 1
      u_45 = nondet "Resonz" (UId 45) AR [u_43,u_44,c_6] 1
      u_46 = nondet "Dust" (UId 46) AR [c_10] 1
      u_47 = binop CS "*" AR u_46 c_9
      u_48 = nondet "Rand" (UId 48) IR [c_8,c_7] 1
      u_49 = nondet "Resonz" (UId 49) AR [u_47,u_48,c_6] 1
      u_50 = nondet "Dust" (UId 50) AR [c_10] 1
      u_51 = binop CS "*" AR u_50 c_9
      u_52 = nondet "Rand" (UId 52) IR [c_8,c_7] 1
      u_53 = nondet "Resonz" (UId 53) AR [u_51,u_52,c_6] 1
      u_54 = nondet "Sum4" (UId 54) AR [u_41,u_45,u_49,u_53] 1
      u_55 = nondet "Dust" (UId 55) AR [c_10] 1
      u_56 = binop CS "*" AR u_55 c_9
      u_57 = nondet "Rand" (UId 57) IR [c_8,c_7] 1
      u_58 = nondet "Resonz" (UId 58) AR [u_56,u_57,c_6] 1
      u_59 = nondet "Dust" (UId 59) AR [c_10] 1
      u_60 = binop CS "*" AR u_59 c_9
      u_61 = nondet "Rand" (UId 61) IR [c_8,c_7] 1
      u_62 = nondet "Resonz" (UId 62) AR [u_60,u_61,c_6] 1
      u_63 = nondet "Dust" (UId 63) AR [c_10] 1
      u_64 = binop CS "*" AR u_63 c_9
      u_65 = nondet "Rand" (UId 65) IR [c_8,c_7] 1
      u_66 = nondet "Resonz" (UId 66) AR [u_64,u_65,c_6] 1
      u_67 = nondet "Dust" (UId 67) AR [c_10] 1
      u_68 = binop CS "*" AR u_67 c_9
      u_69 = nondet "Rand" (UId 69) IR [c_8,c_7] 1
      u_70 = nondet "Resonz" (UId 70) AR [u_68,u_69,c_6] 1
      u_71 = nondet "Sum4" (UId 71) AR [u_58,u_62,u_66,u_70] 1
      u_72 = nondet "Dust" (UId 72) AR [c_10] 1
      u_73 = binop CS "*" AR u_72 c_9
      u_74 = nondet "Rand" (UId 74) IR [c_8,c_7] 1
      u_75 = nondet "Resonz" (UId 75) AR [u_73,u_74,c_6] 1
      u_76 = nondet "Dust" (UId 76) AR [c_10] 1
      u_77 = binop CS "*" AR u_76 c_9
      u_78 = nondet "Rand" (UId 78) IR [c_8,c_7] 1
      u_79 = nondet "Resonz" (UId 79) AR [u_77,u_78,c_6] 1
      u_80 = nondet "Dust" (UId 80) AR [c_10] 1
      u_81 = binop CS "*" AR u_80 c_9
      u_82 = nondet "Rand" (UId 82) IR [c_8,c_7] 1
      u_83 = nondet "Resonz" (UId 83) AR [u_81,u_82,c_6] 1
      u_84 = nondet "Sum4" (UId 84) AR [u_71,u_75,u_79,u_83] 1
      u_85 = nondet "Dust" (UId 85) AR [c_10] 1
      u_86 = binop CS "*" AR u_85 c_9
      u_87 = nondet "Rand" (UId 87) IR [c_8,c_7] 1
      u_88 = nondet "Resonz" (UId 88) AR [u_86,u_87,c_6] 1
      u_89 = nondet "Dust" (UId 89) AR [c_10] 1
      u_90 = binop CS "*" AR u_89 c_9
      u_91 = nondet "Rand" (UId 91) IR [c_8,c_7] 1
      u_92 = nondet "Resonz" (UId 92) AR [u_90,u_91,c_6] 1
      u_93 = nondet "Dust" (UId 93) AR [c_10] 1
      u_94 = binop CS "*" AR u_93 c_9
      u_95 = nondet "Rand" (UId 95) IR [c_8,c_7] 1
      u_96 = nondet "Resonz" (UId 96) AR [u_94,u_95,c_6] 1
      u_97 = nondet "Sum4" (UId 97) AR [u_84,u_88,u_92,u_96] 1
      u_98 = nondet "DelayN" (UId 98) AR [u_97,c_5,c_5] 1
      u_99 = nondet "Rand" (UId 99) IR [c_11,c_4] 1
      u_100 = nondet "LFNoise1" (UId 100) KR [u_99] 1
      u_101 = binop CS "*" KR u_100 c_3
      u_102 = binop CS "+" KR u_101 c_2
      u_103 = nondet "CombL" (UId 103) AR [u_98,c_4,u_102,c_1] 1
      u_104 = nondet "Sum4" (UId 104) AR [u_62,u_66,u_70,u_75] 1
      u_105 = nondet "Sum4" (UId 105) AR [u_104,u_79,u_83,u_88] 1
      u_106 = nondet "Dust" (UId 106) AR [c_10] 1
      u_107 = binop CS "*" AR u_106 c_9
      u_108 = nondet "Rand" (UId 108) IR [c_8,c_7] 1
      u_109 = nondet "Resonz" (UId 109) AR [u_107,u_108,c_6] 1
      u_110 = nondet "Sum4" (UId 110) AR [u_105,u_92,u_96,u_109] 1
      u_111 = nondet "DelayN" (UId 111) AR [u_110,c_5,c_5] 1
      u_112 = nondet "Rand" (UId 112) IR [c_11,c_4] 1
      u_113 = nondet "LFNoise1" (UId 113) KR [u_112] 1
      u_114 = binop CS "*" KR u_113 c_3
      u_115 = binop CS "+" KR u_114 c_2
      u_116 = nondet "CombL" (UId 116) AR [u_111,c_4,u_115,c_1] 1
      u_117 = nondet "Sum4" (UId 117) AR [u_66,u_70,u_75,u_79] 1
      u_118 = nondet "Sum4" (UId 118) AR [u_117,u_83,u_88,u_92] 1
      u_119 = nondet "Dust" (UId 119) AR [c_10] 1
      u_120 = binop CS "*" AR u_119 c_9
      u_121 = nondet "Rand" (UId 121) IR [c_8,c_7] 1
      u_122 = nondet "Resonz" (UId 122) AR [u_120,u_121,c_6] 1
      u_123 = nondet "Sum4" (UId 123) AR [u_118,u_96,u_109,u_122] 1
      u_124 = nondet "DelayN" (UId 124) AR [u_123,c_5,c_5] 1
      u_125 = nondet "Rand" (UId 125) IR [c_11,c_4] 1
      u_126 = nondet "LFNoise1" (UId 126) KR [u_125] 1
      u_127 = binop CS "*" KR u_126 c_3
      u_128 = binop CS "+" KR u_127 c_2
      u_129 = nondet "CombL" (UId 129) AR [u_124,c_4,u_128,c_1] 1
      u_130 = nondet "Sum4" (UId 130) AR [u_70,u_75,u_79,u_83] 1
      u_131 = nondet "Sum4" (UId 131) AR [u_130,u_88,u_92,u_96] 1
      u_132 = nondet "Dust" (UId 132) AR [c_10] 1
      u_133 = binop CS "*" AR u_132 c_9
      u_134 = nondet "Rand" (UId 134) IR [c_8,c_7] 1
      u_135 = nondet "Resonz" (UId 135) AR [u_133,u_134,c_6] 1
      u_136 = nondet "Sum4" (UId 136) AR [u_131,u_109,u_122,u_135] 1
      u_137 = nondet "DelayN" (UId 137) AR [u_136,c_5,c_5] 1
      u_138 = nondet "Rand" (UId 138) IR [c_11,c_4] 1
      u_139 = nondet "LFNoise1" (UId 139) KR [u_138] 1
      u_140 = binop CS "*" KR u_139 c_3
      u_141 = binop CS "+" KR u_140 c_2
      u_142 = nondet "CombL" (UId 142) AR [u_137,c_4,u_141,c_1] 1
      u_143 = nondet "Sum4" (UId 143) AR [u_103,u_116,u_129,u_142] 1
      u_144 = nondet "Sum4" (UId 144) AR [u_75,u_79,u_83,u_88] 1
      u_145 = nondet "Sum4" (UId 145) AR [u_144,u_92,u_96,u_109] 1
      u_146 = nondet "Dust" (UId 146) AR [c_10] 1
      u_147 = binop CS "*" AR u_146 c_9
      u_148 = nondet "Rand" (UId 148) IR [c_8,c_7] 1
      u_149 = nondet "Resonz" (UId 149) AR [u_147,u_148,c_6] 1
      u_150 = nondet "Sum4" (UId 150) AR [u_145,u_122,u_135,u_149] 1
      u_151 = nondet "DelayN" (UId 151) AR [u_150,c_5,c_5] 1
      u_152 = nondet "Rand" (UId 152) IR [c_11,c_4] 1
      u_153 = nondet "LFNoise1" (UId 153) KR [u_152] 1
      u_154 = binop CS "*" KR u_153 c_3
      u_155 = binop CS "+" KR u_154 c_2
      u_156 = nondet "CombL" (UId 156) AR [u_151,c_4,u_155,c_1] 1
      u_157 = nondet "Sum4" (UId 157) AR [u_79,u_83,u_88,u_92] 1
      u_158 = nondet "Sum4" (UId 158) AR [u_157,u_96,u_109,u_122] 1
      u_159 = nondet "Dust" (UId 159) AR [c_10] 1
      u_160 = binop CS "*" AR u_159 c_9
      u_161 = nondet "Rand" (UId 161) IR [c_8,c_7] 1
      u_162 = nondet "Resonz" (UId 162) AR [u_160,u_161,c_6] 1
      u_163 = nondet "Sum4" (UId 163) AR [u_158,u_135,u_149,u_162] 1
      u_164 = nondet "DelayN" (UId 164) AR [u_163,c_5,c_5] 1
      u_165 = nondet "Rand" (UId 165) IR [c_11,c_4] 1
      u_166 = nondet "LFNoise1" (UId 166) KR [u_165] 1
      u_167 = binop CS "*" KR u_166 c_3
      u_168 = binop CS "+" KR u_167 c_2
      u_169 = nondet "CombL" (UId 169) AR [u_164,c_4,u_168,c_1] 1
      u_170 = nondet "Sum4" (UId 170) AR [u_83,u_88,u_92,u_96] 1
      u_171 = nondet "Sum4" (UId 171) AR [u_170,u_109,u_122,u_135] 1
      u_172 = nondet "Dust" (UId 172) AR [c_10] 1
      u_173 = binop CS "*" AR u_172 c_9
      u_174 = nondet "Rand" (UId 174) IR [c_8,c_7] 1
      u_175 = nondet "Resonz" (UId 175) AR [u_173,u_174,c_6] 1
      u_176 = nondet "Sum4" (UId 176) AR [u_171,u_149,u_162,u_175] 1
      u_177 = nondet "DelayN" (UId 177) AR [u_176,c_5,c_5] 1
      u_178 = nondet "Rand" (UId 178) IR [c_11,c_4] 1
      u_179 = nondet "LFNoise1" (UId 179) KR [u_178] 1
      u_180 = binop CS "*" KR u_179 c_3
      u_181 = binop CS "+" KR u_180 c_2
      u_182 = nondet "CombL" (UId 182) AR [u_177,c_4,u_181,c_1] 1
      u_183 = nondet "Sum4" (UId 183) AR [u_143,u_156,u_169,u_182] 1
      u_184 = nondet "RandN" (UId 184) IR [c_11,c_2] 2
      [u_184_o_0,u_184_o_1] = mceChannels u_184
      u_185 = nondet "AllpassN" (UId 185) AR [u_183,c_2,u_184_o_0,c_0] 1
      u_186 = nondet "RandN" (UId 186) IR [c_11,c_2] 2
      [u_186_o_0,u_186_o_1] = mceChannels u_186
      u_187 = nondet "AllpassN" (UId 187) AR [u_185,c_2,u_186_o_0,c_0] 1
      u_188 = nondet "RandN" (UId 188) IR [c_11,c_2] 2
      [u_188_o_0,u_188_o_1] = mceChannels u_188
      u_189 = nondet "AllpassN" (UId 189) AR [u_187,c_2,u_188_o_0,c_0] 1
      u_190 = nondet "RandN" (UId 190) IR [c_11,c_2] 2
      [u_190_o_0,u_190_o_1] = mceChannels u_190
      u_191 = nondet "AllpassN" (UId 191) AR [u_189,c_2,u_190_o_0,c_0] 1
      u_192 = binop CS "*" AR c_10 u_191
      u_193 = binop CS "+" AR u_54 u_192
      u_194 = nondet "AllpassN" (UId 194) AR [u_183,c_2,u_184_o_1,c_0] 1
      u_195 = nondet "AllpassN" (UId 195) AR [u_194,c_2,u_186_o_1,c_0] 1
      u_196 = nondet "AllpassN" (UId 196) AR [u_195,c_2,u_188_o_1,c_0] 1
      u_197 = nondet "AllpassN" (UId 197) AR [u_196,c_2,u_190_o_1,c_0] 1
      u_198 = binop CS "*" AR c_10 u_197
      u_199 = binop CS "+" AR u_54 u_198
      u_200 = nondet "Out" (UId 200) AR [c_11,u_193,u_199] 0
  in u_200
$
~~~~
