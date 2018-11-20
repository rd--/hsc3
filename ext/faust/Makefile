FAUST_RAW = https://raw.githubusercontent.com/grame-cncm/faust/master-dev

mk-get-dsp:
	wget $(FAUST_RAW)/examples/reverb/zitaRev.dsp

mk-ugen:
	faust2supercollider -d zitaRev.dsp

mk-install-ugen:
	cp zitaRev.so ~/opt/lib/SuperCollider/plugins

clean:
	rm -fR faust.*
	rm -f zitaRev.sc zitaRev.so
