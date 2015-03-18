.PHONY: clean all

sources = main.hs File.hs Tree.hs Resource.hs

drive: ${sources}
	ghc -o drive main.hs -i./ -O2 -fllvm -framework Security

clean:
	rm *.o *.hi drive
