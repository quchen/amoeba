MAIN = amoeba
MAIN_BS = bootstrap
SRC = src
OPTIMIZE = -O2
PROF = -prof -auto-all -caf-all

opt :
	ghc $(OPTIMIZE) -i$(SRC) $(SRC)/Main.hs                -o $(MAIN)
	ghc $(OPTIMIZE) -i$(SRC) $(SRC)/MainBootstrapServer.hs -o $(MAIN_BS)

prof :
	ghc $(PROF) $(OPTIMIZE) -i$(SRC) $(SRC)/Main.hs                -o $(MAIN)
	ghc $(PROF) $(OPTIMIZE) -i$(SRC) $(SRC)/MainBootstrapServer.hs -o $(MAIN_BS)

fast :
	ghc -i$(SRC) $(SRC)/Main.hs                -o $(MAIN)
	ghc -i$(SRC) $(SRC)/MainBootstrapServer.hs -o $(MAIN_BS)

clean :
	find $(SRC) -name "*.hi" -delete
	find $(SRC) -name "*.o" -delete
	rm -f $(MAIN)
	rm -f $(MAIN_BS)
