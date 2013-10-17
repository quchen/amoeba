MAIN = amoeba
BOOTSTRAP = bootstrap
SRC = src
HSFLAGS =

all :
	ghc -o $(MAIN)      $(HSFLAGS) -i$(SRC) $(SRC)/Main.hs
	ghc -o $(BOOTSTRAP) $(HSFLAGS) -i$(SRC) $(SRC)/MainBootstrapServer.hs

bootstrap : all
	./$(BSMAIN)

clean :
	rm -f $(SRC)/*.o
	rm -f $(SRC)/*.hi
	rm -f $(MAIN)
	rm -f $(BOOTSTRAP)
