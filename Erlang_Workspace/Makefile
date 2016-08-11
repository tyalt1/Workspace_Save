.PHONY: all clean

FILES = $(wildcard ./*.erl)

all: $(FILES)
	erlc $^

shell: all
	@erl

clean:
	rm -f *.beam
