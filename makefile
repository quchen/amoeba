MAIN = amoeba
SRC = src

all :
	ghc -O -o $(MAIN) -i$(SRC) $(SRC)/Main.hs

bootstrap : all
	./$(BSMAIN)

clean :
	rm -f $(SRC)/*.o
	rm -f $(SRC)/*.hi
	rm -f $(MAIN)
