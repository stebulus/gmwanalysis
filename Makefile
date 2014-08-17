.DELETE_ON_ERROR:

include dates.mk
dates.mk: dates
	awk '{print "DATES += " $$0}' $< >$@
dates: mkdates
	./mkdates >$@

words: $(foreach d,${DATES},joon/$d.word mike/$d.word)
	cat $^ |sort >$@
%.word: %.html
	grep '^<p>Select to reveal answer word: ' $< |sed -e 's,^.*<span[^>]*>,$* ,' -e 's,</span>.*$$,,' -e 's,/, ,' >$@
training-set-%: words
	awk '$$1=="$*" && $$2<"2014" {print $$3}' $< >$@
testing-set-%: words
	awk '$$1=="$*" && $$2>="2014" {print $$3}' $< >$@

ALPHABET=a b c d e f g h i j k l m n o p q r s t u v w x y z
freq: $(foreach L,$(ALPHABET),ngram/googlebooks-eng-all-1gram-20120701-$(L).twl)
	cat $+ >$@
ngram/googlebooks-eng-all-1gram-20120701-%.twl: ngram/googlebooks-eng-all-1gram-20120701-%.gz twl
	zcat $< | ngram/shorten | sort >$@
ngram/googlebooks-eng-all-1gram-20120701-%.gz:
	wget -P ngram http://storage.googleapis.com/books/ngrams/books/googlebooks-eng-all-1gram-20120701-$*.gz
logfreq: freq
	awk '{print $$1, log($$2)}' $< >$@

wikt/enwikt.xml.bz2:
	wget -O "$@" http://dumps.wikimedia.org/enwiktionary/20140504/enwiktionary-20140504-pages-articles-multistream.xml.bz2
wikt/reduced: wikt/reduce wikt/enwikt.xml.bz2 twl
	bzcat wikt/enwikt.xml.bz2 \
        | xml2 \
        | egrep '^/mediawiki/page($$|/title=|/ns=|/revision/text=.)' \
        | sed 's,^/mediawiki/page,,' \
        | wikt/reduce twl \
        > $@
wikt/reduce: wikt/reduce.hs
	ghc -O $<
wikt/etyls: wikt/reduced
	grep -o "{{etyl|[^|]*|en}}" $< |sort |uniq -c |awk '$$1>=100 {print $$2}' |sed 's,[\\|.{}],\\&,g' >$@

wordlists/%: wordlists/%.hs
	ghc -W $<
wordlists/%.set: wordlists/%.html wordlists/%
	wordlists/$* $< >$@

SETS += wordlists/gre.set
wordlists/gre.set: wordlists/kaplan-gre.set wordlists/major-gre.set
	cat $^ |sort -u >$@

wordlists/kaplan-gre.html:
	wget -O "$@" http://quizlet.com/216464/kaplan-gre-vocabulary-flash-cards/

wordlists/major-gre-%-iso8859-1.html:
	wget -O "$@" http://www.majortests.com/gre/wordlist_$*
wordlists/major-gre-%.set: wordlists/major-gre-%.html wordlists/major
	wordlists/major $< >$@
MAJOR_GRE_ALL = $(foreach N,$(shell seq -f%02.0f 1 15),wordlists/major-gre-$N.set)
wordlists/major-gre.set: $(MAJOR_GRE_ALL)
	cat $^ >$@

SETS += wordlists/major-sat.set
wordlists/major-sat-%-iso8859-1.html:
	wget -O "$@" http://www.majortests.com/sat/wordlist-$*
wordlists/major-sat-%.set: wordlists/major-sat-%.html wordlists/major
	wordlists/major $< >$@
MAJOR_SAT_ALL = $(foreach N,$(shell seq -f%02.0f 1 10) $(shell seq -f%02.0f 16 25),wordlists/major-sat-$N.set)
wordlists/major-sat.set: $(MAJOR_SAT_ALL)
	cat $^ >$@

wordlists/major-%.html: wordlists/major-%-iso8859-1.html
	iconv -f ISO-8859-1 -t UTF-8 $< >$@

include wikt/all-macro-patterns.mk
wikt/all-macro-patterns.mk: wikt/all-macro-patterns
	tr -dc 'a-zA-Z0-9\n' <$< |paste - $< \
	  |awk -F'\t' '{print "wikt/" $$1 ".set: wikt/reduced"; \
          print "\tegrep -e \"" $$2 "\" $$< |cut -d\\  -f1 >$$@"; \
	  print "SETS += wikt/" $$1 ".set" }' >$@
wikt/all-macro-patterns: wikt/macro-patterns wikt/etyls
	cat $^ >$@

analyze : analyze.hs Tree.hs
	ghc -O2 -W $<
test-weights-% : analyze twl training-set-% logfreq $(SETS)
	/usr/bin/time ./analyze twl training-set-$* logfreq $(SETS) >$@
