# Filenames
MAIN_NODE = amoeba
MAIN_BS = bootstrap
SRC = src
DOC = doc

# Flags
OPTIMIZE = -O2
PROF = -prof -auto-all -caf-all
OPTIONS = -hide-package monads-tf



# Release quality build
.PHONY : release
release :
	ghc $(OPTIONS) $(OPTIMIZE) -i$(SRC) -o $(MAIN_NODE) $(SRC)/Main.hs
	ghc $(OPTIONS) $(OPTIMIZE) -i$(SRC) -o $(MAIN_BS) $(SRC)/MainBootstrapServer.hs


# Fully optimize with profiling support
.PHONY : prof
prof :
	ghc $(OPTIMIZE) $(PROF) $(OPTIONS) -i$(SRC) -o $(MAIN_NODE) $(SRC)/Main.hs
	ghc $(OPTIMIZE) $(PROF) $(OPTIONS) -i$(SRC) -o $(MAIN_BS) $(SRC)/MainBootstrapServer.hs


# Minimize compilation time
.PHONY : fast
fast :
	ghc $(OPTIONS) -i$(SRC) -o $(MAIN_NODE) $(SRC)/Main.hs
	ghc $(OPTIONS) -i$(SRC) -o $(MAIN_BS) $(SRC)/MainBootstrapServer.hs


# Documentation
.PHONY : doc
doc :
	cat $(DOC)/information_flow.dot | cpp | dot -Tpng > $(DOC)/information_flow.png
	cat $(DOC)/network.dot          | cpp | neato -Tpng > $(DOC)/network.png



.PHONY : clean
clean :
	find $(SRC) -name "*.hi" -delete
	find $(SRC) -name "*.o" -delete
	rm -f $(MAIN_NODE)
	rm -f $(MAIN_BS)
	rm -f $(DOC)/information_flow.png
	rm -f $(DOC)/network.png
