################################################################################
###  CONFIGURATION  ############################################################
################################################################################



# Environment
NUM_CORES=$(shell grep -c ^processor /proc/cpuinfo)

# Executable filenames
MAIN_NODE=amoeba
MAIN_MULTI=amoeba_multi
MAIN_BS=bootstrap
MAIN_DRAW=drawing


# Directories
SRC-D=src
MAIN-D=$(SRC-D)/Main
DOC-D=doc
PACKAGE-D=$(shell find .cabal-sandbox/ -name "*packages.conf.d")

# GHC flags
PARALLEL_GHC=-j$(NUM_CORES)
OPTIMIZE=-O2
PROF=-prof -auto-all -caf-all
WARN=-Wall -fno-warn-type-defaults -fno-warn-unused-do-bind -fwarn-tabs -fwarn-incomplete-uni-patterns
PACKAGEDB=-no-user-package-db -package-db $(PACKAGE-D)
THREADED=-threaded

# Cabal flags
PARALLEL_CABAL=-j$(NUM_CORES)

# Executables
CABAL=cabal
GHC=ghc $(THREADED) $(PARALLEL_GHC) -i$(SRC-D) $(WARN) $(PACKAGEDB)
HLINT=hlint --colour
PAGER=less -R
SHELL=bash



################################################################################
### SCRIPT - here be dragons  ##################################################
################################################################################



.PHONY : noop
noop:
	@echo "No target specified. Possible choices:"

	@ # Taken from http://stackoverflow.com/a/9524878/1106679
	@ # Display all build targets in this makefile
	@make -qp | awk -F':' '/^[a-zA-Z0-9][^$$#\/\t=]*:([^=]|$$)/ {split($$1,A,/ /);for(i in A)print A[i]}'




# Set cabal/sandbox up
.PHONY : cabal-init
cabal-init : cabal-update cabal-noupdate



# Done automatically by Travis, provided for manual calls to cabal-init
.PHONY : cabal-update
cabal-update :
	$(CABAL) update



.PHONY : cabal-noupdate
cabal-noupdate :
	$(CABAL) sandbox init
	$(CABAL) install $(PARALLEL_CABAL) --only-dependencies --ghc-options=-w
	$(CABAL) configure



# Release quality build
RELEASE_FLAGS=$(OPTIMIZE)
.PHONY : release
release :
	@echo -e "\e[32mSingle client\e[0m"
	$(GHC) $(RELEASE_FLAGS) -o $(MAIN_NODE) $(MAIN-D)/NodeExecutable.hs
	@echo -e "\e[32mMultiple clients\e[0m"
	$(GHC) $(RELEASE_FLAGS) -o $(MAIN_MULTI) $(MAIN-D)/MultiExecutable.hs
	@echo -e "\e[32mBootstrap server\e[0m"
	$(GHC) $(RELEASE_FLAGS) -o $(MAIN_BS) $(MAIN-D)/BootstrapExecutable.hs
	@echo -e "\e[32mDrawing server\e[0m"
	$(GHC) $(RELEASE_FLAGS) -o $(MAIN_DRAW) $(MAIN-D)/DrawingExecutable.hs


# Fully optimize with profiling support
PROF_FLAGS=$(OPTIMIZE) $(PROF)
.PHONY : prof
prof :
	@echo -e "\e[32mSingle client\e[0m"
	$(GHC) $(PROF_FLAGS) -o $(MAIN_NODE) $(MAIN-D)/NodeExecutable.hs
	@echo -e "\e[32mMultiple clients\e[0m"
	$(GHC) $(PROF_FLAGS) -o $(MAIN_MULTI) $(MAIN-D)/MultiExecutable.hs
	@echo -e "\e[32mBootstrap server\e[0m"
	$(GHC) $(PROF_FLAGS) -o $(MAIN_BS) $(MAIN-D)/BootstrapExecutable.hs
	@echo -e "\e[32mDrawing server\e[0m"
	$(GHC) $(PROF_FLAGS) -o $(MAIN_DRAW) $(MAIN-D)/DrawingExecutable.hs


# Minimize compilation time
FAST_FLAGS=
.PHONY : fast
fast :
	@echo -e "\e[32mSingle client\e[0m"
	$(GHC) $(FAST_FLAGS) -o $(MAIN_NODE) $(MAIN-D)/NodeExecutable.hs
	@echo -e "\e[32mMultiple clients\e[0m"
	$(GHC) $(FAST_FLAGS) -o $(MAIN_MULTI) $(MAIN-D)/MultiExecutable.hs
	@echo -e "\e[32mBootstrap server\e[0m"
	$(GHC) $(FAST_FLAGS) -o $(MAIN_BS) $(MAIN-D)/BootstrapExecutable.hs
	@echo -e "\e[32mDrawing server\e[0m"
	$(GHC) $(FAST_FLAGS) -o $(MAIN_DRAW) $(MAIN-D)/DrawingExecutable.hs



# Typecheck and warn, but don't link
NOLINK_FLAGS=-no-link
.PHONY : nolink
nolink :
	@echo -e "\e[32mSingle client\e[0m"
	$(GHC) $(NOLINK_FLAGS) -o $(MAIN_NODE) $(MAIN-D)/NodeExecutable.hs
	@echo -e "\e[32mMultiple clients\e[0m"
	$(GHC) $(NOLINK_FLAGS) -o $(MAIN_MULTI) $(MAIN-D)/MultiExecutable.hs
	@echo -e "\e[32mBootstrap server\e[0m"
	$(GHC) $(NOLINK_FLAGS) -o $(MAIN_BS) $(MAIN-D)/BootstrapExecutable.hs
	@echo -e "\e[32mDrawing server\e[0m"
	$(GHC) $(NOLINK_FLAGS) -o $(MAIN_DRAW) $(MAIN-D)/DrawingExecutable.hs



# Documentation
.PHONY : doc
doc :
	cat $(DOC-D)/information_flow.dot | cpp | dot -Tpng > $(DOC-D)/information_flow.png
	cat $(DOC-D)/network.dot | cpp | neato -Tpng > $(DOC-D)/network.png


# HLint
.PHONY : hlint
hlint :
	find $(SRC-D) -name "*.hs" -print0 | xargs -0 $(HLINT) | $(PAGER)


.PHONY : clean
clean :
	find $(SRC-D) -name "*.hi" -delete
	find $(SRC-D) -name "*.o" -delete
	rm -f $(MAIN_NODE)
	rm -f $(MAIN_MULTI)
	rm -f $(MAIN_BS)
	rm -f $(MAIN_DRAW)