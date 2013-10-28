MAIN = amoeba
MAIN_BS = bootstrap
SRC = src
HSFLAGS =
PROF =

all :
	ghc $(HSFLAGS) -i$(SRC) $(SRC)/Main.hs                -o $(MAIN)
	ghc $(HSFLAGS) -i$(SRC) $(SRC)/MainBootstrapServer.hs -o $(MAIN_BS)

prof :
	ghc $(PROF) $(HSFLAGS) -i$(SRC) $(SRC)/Main.hs                -o $(MAIN)
	ghc $(PROF) $(HSFLAGS) -i$(SRC) $(SRC)/MainBootstrapServer.hs -o $(MAIN_BS)

clean :
	rm -f $(SRC)/*.o
	rm -f $(SRC)/*.hi
	rm -f $(MAIN)
	rm -f $(MAIN_BS)
