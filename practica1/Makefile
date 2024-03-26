
test: test-python test-haskell

test-python: gerarquia.py
	./python-checker.sh

gerarquia: gerarquia.hs
	ghc $<

test-haskell: gerarquia
	./haskell-checker.sh

zip:
	$(RM) tests.zip
	zip -r tests.zip Makefile haskell-checker.sh python-checker.sh tests
