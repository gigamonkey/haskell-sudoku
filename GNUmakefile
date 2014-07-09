source = $(wildcard *.hs)
binaries = $(basename $(source))

all: $(binaries)

test: sudoku
	./sudoku *.txt

clean: tidy
	rm -f $(binaries)
	rm -f *.hi
	rm -f *.o

tidy:
	rm -f *~

%: %.hs
	ghc -O2 $<
