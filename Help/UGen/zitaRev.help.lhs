http://kokkinizita.linuxaudio.org/linuxaudio/zita-rev1-doc/quickguide.html

> import Sound.SC3 {- hsc3 -}
> import qualified Sound.SC3.UGen.Bindings.HW.External.Zita as Zita {- hsc3 -}

    > Zita.zitaRev_param

mostly default settings

> g_01 =
>     let i = soundIn 0
>         in_delay = 60
>         eq1_freq = 315
>         eq2_freq = 1500
>         dry_wet_mix = 0.5
>         level = 0
>     in Zita.zitaRev i i in_delay 200 3 2 6000 eq1_freq 0 eq2_freq 0 dry_wet_mix level

longer

> g_02 =
>     let i = soundIn 0
>         in_delay = 80
>         low_rt60 = 6
>         mid_rt60 = 4
>         eq1_freq = 190
>         eq1_level = -6
>         eq2_freq = 3500
>         eq2_level = 6
>         dry_wet_mix = 0
>         level = 0
>     in Zita.zitaRev i i in_delay 200 low_rt60 mid_rt60 6000 eq1_freq eq1_level eq2_freq eq2_level dry_wet_mix level

longer still

> g_03 =
>     let i = soundIn 0
>         in_delay = 100
>         low_rt60 = 6
>         mid_rt60 = 8
>         eq1_freq = 190
>         eq1_level = -6
>         eq2_freq = 3500
>         eq2_level = 6
>         dry_wet_mix = 0.5
>         level = 0.0
>     in Zita.zitaRev i i in_delay 200 low_rt60 mid_rt60 6000 eq1_freq eq1_level eq2_freq eq2_level dry_wet_mix level


