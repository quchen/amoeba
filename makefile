MAIN_NODE = amoeba
MAIN_BS = bootstrap
SRC = src
OPTIMIZE = -O2
PROF = -prof -auto-all -caf-all
OPTIONS = -hide-package monads-tf

opt :
	ghc $(OPTIONS) $(OPTIMIZE) -i$(SRC) -o $(MAIN_NODE) $(SRC)/Main.hs
	ghc $(OPTIONS) $(OPTIMIZE) -i$(SRC) -o $(MAIN_BS) $(SRC)/MainBootstrapServer.hs

prof :
	ghc $(OPTIMIZE) $(PROF) $(OPTIONS) -i$(SRC) -o $(MAIN_NODE) $(SRC)/Main.hs
	ghc $(OPTIMIZE) $(PROF) $(OPTIONS) -i$(SRC) -o $(MAIN_BS) $(SRC)/MainBootstrapServer.hs

fast :
	ghc $(OPTIONS) -i$(SRC) -o $(MAIN_NODE) $(SRC)/Main.hs
	ghc $(OPTIONS) -i$(SRC) -o $(MAIN_BS) $(SRC)/MainBootstrapServer.hs

clean :
	find $(SRC) -name "*.hi" -delete
	find $(SRC) -name "*.o" -delete
	rm -f $(MAIN_NODE)
	rm -f $(MAIN_BS)
