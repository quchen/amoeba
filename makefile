# Filenames
MAIN_NODE = amoeba
MAIN_BS = bootstrap
MAIN_DRAW = bootstrap
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


SHELL = bash





# Release quality build
RELEASE_FLAGS = $(OPTIMIZE) $(WARN)
.PHONY : release
release :
	@echo -e "\e[32mSingle client\e[0m"
	$(GHC) $(RELEASE_FLAGS) -o $(MAIN_NODE) $(SRC)/Main.hs
	@echo -e "\e[32mBootstrap server\e[0m"
	$(GHC) $(RELEASE_FLAGS) -o $(MAIN_BS)   $(SRC)/MainBootstrapServer.hs
	@echo -e "\e[32mDrawing server\e[0m"
	$(GHC) $(RELEASE_FLAGS) -o $(MAIN_DRAW) $(SRC)/MainDrawingServer.hs


# Fully optimize with profiling support
PROF_FLAGS = $(OPTIMIZE) $(PROF)
.PHONY : prof
prof :
	@echo -e "\e[32mSingle client\e[0m"
	$(GHC) $(PROF_FLAGS) -o $(MAIN_NODE) $(SRC)/Main.hs
	@echo -e "\e[32mBootstrap server\e[0m"
	$(GHC) $(PROF_FLAGS) -o $(MAIN_BS)   $(SRC)/MainBootstrapServer.hs
	@echo -e "\e[32mDrawing server\e[0m"
	$(GHC) $(PROF_FLAGS) -o $(MAIN_DRAW) $(SRC)/MainDrawingServer.hs


# Minimize compilation time
FAST_FLAGS =
.PHONY : fast
fast :
	@echo -e "\e[32mSingle client\e[0m"
	$(GHC) $(FAST_FLAGS) -o $(MAIN_NODE) $(SRC)/Main.hs
	@echo -e "\e[32mBootstrap server\e[0m"
	$(GHC) $(FAST_FLAGS) -o $(MAIN_BS)   $(SRC)/MainBootstrapServer.hs
	@echo -e "\e[32mDrawing server\e[0m"
	$(GHC) $(FAST_FLAGS) -o $(MAIN_DRAW) $(SRC)/MainDrawingServer.hs


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
	rm -f $(MAIN_DRAW)
	rm -f $(DOC)/information_flow.png
	rm -f $(DOC)/network.png
