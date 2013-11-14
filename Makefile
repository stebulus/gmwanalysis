include dates.mk
dates.mk: dates
	awk '{print "DATES += " $$0}' $< >$@
dates: mkdates
	./mkdates >$@

words: $(foreach d,${DATES},joon/$d.word mike/$d.word)
	cat $^ |sort >$@
%.word: %.html
	grep '^<p>Select to reveal answer word: ' $< |sed -e 's,^.*<span[^>]*>,$* ,' -e 's,</span>.*$$,,' -e 's,/, ,' >$@
