

tunnel: tunnel.hs
	ghc $<

test: tunnel.py
	python3 checker.py 5

zip:
	$(RM) scripts.zip
	zip -r scripts.zip Makefile checker.py tunnel.py generator.py
