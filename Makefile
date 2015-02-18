.PHONY: clean all

drive: main.hs OAuth2.hs CSRFToken.hs URI.hs Token.hs ConfigFile.hs
	ghc -o drive main.hs -i./

clean:
	rm *.o *.hi drive
