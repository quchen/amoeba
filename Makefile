# Filenames
MAIN_NODE = amoeba
MAIN_MULTI = amoeba_multi
MAIN_BS = bootstrap
MAIN_DRAW = drawing

# Directories
SRCDIR = src
MAINDIR=$(SRCDIR)/Main
DOCDIR = doc
PACKAGEDIR = $(shell find .cabal-sandbox/ -name "*packages.conf.d")


# GHC Flags
OPTIMIZE = -O2 -threaded
PROF = -prof -auto-all -caf-all -threaded
WARN = -W
OPTIONS =

# Executables
GHC = ghc -i$(SRCDIR) $(OPTIONS) -no-user-package-db -package-db $(PACKAGEDIR)
HLINT = hlint --colour
PAGER = less -R
SHELL = bash



.PHONY : noop
noop:
	@echo "NOOP makefile target. Check the makefile for nontrivial targets."



# Set cabal/sandbox up
NUM_CORES=$(shell grep -c ^processor /proc/cpuinfo)
.PHONY : cabal-init
cabal-init:
	cabal update
	cabal sandbox init
	cabal install -j$(NUM_CORES) --only-dependencies --ghc-options=-w
	cabal configure



# Release quality build
RELEASE_FLAGS = $(OPTIMIZE) $(WARN)
.PHONY : release
release :
	@echo -e "\e[32mSingle client\e[0m"
	$(GHC) $(RELEASE_FLAGS) -o $(MAIN_NODE) $(MAINDIR)/NodeExecutable.hs
	@echo -e "\e[32mMultiple clients\e[0m"
	$(GHC) $(RELEASE_FLAGS) -o $(MAIN_MULTI) $(MAINDIR)/MultiExecutable.hs
	@echo -e "\e[32mBootstrap server\e[0m"
	$(GHC) $(RELEASE_FLAGS) -o $(MAIN_BS) $(MAINDIR)/BootstrapExecutable.hs
	@echo -e "\e[32mDrawing server\e[0m"
	$(GHC) $(RELEASE_FLAGS) -o $(MAIN_DRAW) $(MAINDIR)/DrawingExecutable.hs


# Fully optimize with profiling support
PROF_FLAGS = $(OPTIMIZE) $(PROF)
.PHONY : prof
prof :
	@echo -e "\e[32mSingle client\e[0m"
	$(GHC) $(PROF_FLAGS) -o $(MAIN_NODE) $(MAINDIR)/NodeExecutable.hs
	@echo -e "\e[32mMultiple clients\e[0m"
	$(GHC) $(PROF_FLAGS) -o $(MAIN_MULTI) $(MAINDIR)/MultiExecutable.hs
	@echo -e "\e[32mBootstrap server\e[0m"
	$(GHC) $(PROF_FLAGS) -o $(MAIN_BS) $(MAINDIR)/BootstrapExecutable.hs
	@echo -e "\e[32mDrawing server\e[0m"
	$(GHC) $(PROF_FLAGS) -o $(MAIN_DRAW) $(MAINDIR)/DrawingExecutable.hs


# Minimize compilation time
FAST_FLAGS =
.PHONY : fast
fast :
	@echo -e "\e[32mSingle client\e[0m"
	$(GHC) $(FAST_FLAGS) -o $(MAIN_NODE) $(MAINDIR)/NodeExecutable.hs
	@echo -e "\e[32mMultiple clients\e[0m"
	$(GHC) $(FAST_FLAGS) -o $(MAIN_MULTI) $(MAINDIR)/MultiExecutable.hs
	@echo -e "\e[32mBootstrap server\e[0m"
	$(GHC) $(FAST_FLAGS) -o $(MAIN_BS) $(MAINDIR)/BootstrapExecutable.hs
	@echo -e "\e[32mDrawing server\e[0m"
	$(GHC) $(FAST_FLAGS) -o $(MAIN_DRAW) $(MAINDIR)/DrawingExecutable.hs



# Typecheck and warn, but don't link
NOLINK_FLAGS = $(WARN) -no-link
.PHONY : nolink
nolink :
	@echo -e "\e[32mSingle client\e[0m"
	$(GHC) $(NOLINK_FLAGS) -o $(MAIN_NODE) $(MAINDIR)/NodeExecutable.hs
	@echo -e "\e[32mMultiple clients\e[0m"
	$(GHC) $(NOLINK_FLAGS) -o $(MAIN_MULTI) $(MAINDIR)/MultiExecutable.hs
	@echo -e "\e[32mBootstrap server\e[0m"
	$(GHC) $(NOLINK_FLAGS) -o $(MAIN_BS) $(MAINDIR)/BootstrapExecutable.hs
	@echo -e "\e[32mDrawing server\e[0m"
	$(GHC) $(NOLINK_FLAGS) -o $(MAIN_DRAW) $(MAINDIR)/DrawingExecutable.hs



# Documentation
.PHONY : doc
doc :
	cat $(DOCDIR)/information_flow.dot | cpp | dot -Tpng > $(DOCDIR)/information_flow.png
	cat $(DOCDIR)/network.dot | cpp | neato -Tpng > $(DOCDIR)/network.png


# HLint
.PHONY : hlint
hlint :
	find $(SRCDIR) -name "*.hs" | xargs $(HLINT) | $(PAGER)


.PHONY : clean
clean :
	find $(SRCDIR) -name "*.hi" -delete
	find $(SRCDIR) -name "*.o" -delete
	rm -f $(MAIN_NODE)
	rm -f $(MAIN_MULTI)
	rm -f $(MAIN_BS)
	rm -f $(MAIN_DRAW)