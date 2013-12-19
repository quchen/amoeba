# Filenames
MAIN_NODE = amoeba
MAIN_BS = bootstrap
SRC = src
DOC = doc


# GHC Flags
OPTIMIZE = -O2 -threaded
PROF = -prof -auto-all -caf-all -threaded
WARN = -W
OPTIONS = -hide-package monads-tf

# Executables
GHC = ghc -i$(SRC) $(OPTIONS)
HLINT = hlint --colour
PAGER = less -R





# Release quality build
RELEASE_FLAGS = $(OPTIMIZE) $(WARN)
.PHONY : release
release :
	$(GHC) $(RELEASE_FLAGS) -o $(MAIN_NODE) $(SRC)/Main.hs
	$(GHC) $(RELEASE_FLAGS) -o $(MAIN_BS) $(SRC)/MainBootstrapServer.hs


# Fully optimize with profiling support
PROF_FLAGS = $(OPTIMIZE) $(PROF)
.PHONY : prof
prof :
	$(GHC) $(PROF_FLAGS) -o $(MAIN_NODE) $(SRC)/Main.hs
	$(GHC) $(PROF_FLAGS) -o $(MAIN_BS) $(SRC)/MainBootstrapServer.hs


# Minimize compilation time
FAST_FLAGS =
.PHONY : fast
fast :
	$(GHC) $(FAST_FLAGS) -o $(MAIN_NODE) $(SRC)/Main.hs
	$(GHC) $(FAST_FLAGS) -o $(MAIN_BS) $(SRC)/MainBootstrapServer.hs


# Documentation
.PHONY : doc
doc :
	cat $(DOC)/information_flow.dot | cpp | dot -Tpng > $(DOC)/information_flow.png
	cat $(DOC)/network.dot          | cpp | neato -Tpng > $(DOC)/network.png


# HLint
.PHONY : hlint
hlint :
	$(HLINT) $(SRC)/*.hs | $(PAGER)


.PHONY : clean
clean :
	find $(SRC) -name "*.hi" -delete
	find $(SRC) -name "*.o" -delete
	rm -f $(MAIN_NODE)
	rm -f $(MAIN_BS)
	rm -f $(DOC)/information_flow.png
	rm -f $(DOC)/network.png
