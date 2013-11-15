MAIN_NODE = amoeba
MAIN_BS = bootstrap
SRC = src
OPTIMIZE = -O2
PROF = -prof -auto-all -caf-all

opt :
	ghc $(OPTIMIZE) -i$(SRC) -o $(MAIN_NODE) $(SRC)/Main.hs
	ghc $(OPTIMIZE) -i$(SRC) -o $(MAIN_BS) $(SRC)/MainBootstrapServer.hs

prof :
	ghc $(PROF) $(OPTIMIZE) -i$(SRC) -o $(MAIN_NODE) $(SRC)/Main.hs
	ghc $(PROF) $(OPTIMIZE) -i$(SRC) -o $(MAIN_BS) $(SRC)/MainBootstrapServer.hs

fast :
	ghc -i$(SRC) -o $(MAIN_NODE) $(SRC)/Main.hs
	ghc -i$(SRC) -o $(MAIN_BS) $(SRC)/MainBootstrapServer.hs

clean :
	find $(SRC) -name "*.hi" -delete
	find $(SRC) -name "*.o" -delete
	rm -f $(MAIN_NODE)
	rm -f $(MAIN_BS)
