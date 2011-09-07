%.html : %.lhs
	pandoc -f markdown+lhs -t html -c hsc3.css --smart < $< > $*.html

all:
	make hsc3.help.html
	make tutorial.html

clean:
	rm -f *.html
