.PHONY: all clean shell

FILES = $(wildcard ./*.ex)

all: $(FILES)
	elixirc $^

shell: all
	@iex

clean:
	rm -f *.beam
