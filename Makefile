.PHONY: clean all

drive: main.hs OAuth2.hs CSRFToken.hs URI.hs Token.hs ConfigFile.hs Util.hs
	ghc -o drive main.hs -i./ -O2 -fllvm

clean:
	rm *.o *.hi drive
