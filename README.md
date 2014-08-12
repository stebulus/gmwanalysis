gmwanalysis
===========

A machine-learning approach to constructing a model for [Guess My
Word!][1]  This game is simply binary search to find words chosen
daily by Joon Pahk and his comrade Mike; the challenge for a computer
player is to model Joon's and Mike's preferences for words.

Run `make test-weights` to generate a model as a test.
(**WARNING**: This will download several gigabytes of data from
Google Ngrams and from Wiktionary.)  The analysis proper (see
[`analyze.hs`](./analyze.hs) is a greedy forward feature selection
algorithm, with features representing (a) the word's level of
commonness/obscurity (as proxied by its frequency in the ngram data)
and (b) some linguistic categories (based on macros used in the word's
Wiktionary page).  At the moment, no cross-validation is done —
that is next!

[1]: http://www.people.fas.harvard.edu/~pahk/dictionary/guess.cgi
