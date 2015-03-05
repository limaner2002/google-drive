.PHONY: clean all

sources = main.hs OAuth2.hs CSRFToken.hs URI.hs Token.hs ConfigFile.hs Util.hs \
	  File.hs external/keychain.c Tree.hs Resource.hs

drive: ${sources}
	ghc -o drive main.hs -i./ -O2 -fllvm -framework Security external/keychain.c

clean:
	rm *.o *.hi drive
