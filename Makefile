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

intlogfreq: freq
	awk '{print $$1, int(log($$2))}' $< >$@
logfreq/%.set: intlogfreq
	awk '$$2 <= $* {print $$1}' $< >$@
logfreq.mk: intlogfreq
	seq $$(awk 'NR==1 {m=M=$$2} $$2<m {m=$$2} $$2>M {M=$$2} END {print m, M}' $<) \
	| sed -e 's,^,SETS += logfreq/,' -e 's,$$,.set,' >$@
include logfreq.mk

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
	grep -o "{{etyl|[^|]*|en}}" $< |sort |uniq -c |awk '$$1>=100 {print $$2}' >$@

include wikt/macro-patterns.mk
wikt/macro-patterns.mk: wikt/macro-patterns
	tr -dc 'a-zA-Z0-9\n' <$< |paste - $< \
	  |awk -F'\t' '{print "wikt/" $$1 ".set: wikt/reduced"; \
          print "\tegrep -e \"" $$2 "\" $$< |cut -d\\  -f1 >$$@"; \
	  print "SETS += wikt/" $$1 ".set" }' >$@

analyze : analyze.hs
	ghc -O2 -W $<
test-weights-% : analyze twl training-set-% $(SETS)
	/usr/bin/time ./analyze training-set-$* $(SETS) >$@
