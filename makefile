MAIN = amoeba
BOOTSTRAP = bootstrap
SRC = src

all :
	ghc -O -o $(MAIN) -i$(SRC) $(SRC)/Main.hs
	ghc -O -o $(BOOTSTRAP) -i$(SRC) $(SRC)/MainBootstrapServer.hs

bootstrap : all
	./$(BSMAIN)

clean :
	rm -f $(SRC)/*.o
	rm -f $(SRC)/*.hi
	rm -f $(MAIN)
	rm -f $(BOOTSTRAP)
