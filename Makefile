reptile: *.hs
	ghc Main.hs -o reptile

# Haddock is awesome <3
doc: *.hs
	haddock -o doc -h --quickjump *.hs
	echo 'To see the doc run "cd doc; php -S localhost:4200" (without server quickjump may not work)'
