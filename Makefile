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

ALPHABET=a b c d e f g h i j k l m n o p q r s t u v w x y z
freq: $(foreach L,$(ALPHABET),ngram/googlebooks-eng-all-1gram-20120701-$(L).twl)
	cat $+ >$@
ngram/googlebooks-eng-all-1gram-20120701-%.twl: ngram/googlebooks-eng-all-1gram-20120701-%.gz twl
	zcat $< | ngram/shorten | sort >$@
ngram/googlebooks-eng-all-1gram-20120701-%.gz:
	wget -P ngram http://storage.googleapis.com/books/ngrams/books/googlebooks-eng-all-1gram-20120701-$*.gz

wikt/enwikt.xml.bz2:
	wget -O "$@" http://dumps.wikimedia.org/enwiktionary/20140504/enwiktionary-20140504-pages-articles-multistream.xml.bz2
