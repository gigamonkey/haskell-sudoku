source = $(wildcard *.hs)
binaries = $(basename $(source))

TEST_PUZZLES := one.txt

all: $(binaries)

test: sudoku
	./sudoku $(TEST_PUZZLES)

lint:
	hlint $(source)

clean: tidy
	rm -f $(binaries)
	rm -f *.hi
	rm -f *.o

tidy:
	rm -f *~

%: %.hs
	ghc -O2 $<
