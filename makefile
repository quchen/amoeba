MAIN = amoeba
BSMAIN = bootstrap
SRC = .

all :
	ghc -O -o $(MAIN) -i$(SRC) $(SRC)/Main.hs
	ghc -O -o $(BSMAIN) -i$(SRC) $(SRC)/BootstrapServerMain.hs

bootstrap : all
	./$(BSMAIN)

clean :
	rm -rf *.o
	rm -rf *.hi
	rm -f $(MAIN)
	rm -f $(BSMAIN)