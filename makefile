MAIN = amoeba
BSMAIN = bootstrap
SRC = src

all :
	ghc -O -o $(MAIN) -i$(SRC) $(SRC)/Main.hs
	ghc -O -o $(BSMAIN) -i$(SRC) $(SRC)/BootstrapServerMain.hs

bootstrap : all
	./$(BSMAIN)

clean :
	rm -f $(SRC)/*.o
	rm -f $(SRC)/*.hi
	rm -f $(MAIN)
	rm -f $(BSMAIN)