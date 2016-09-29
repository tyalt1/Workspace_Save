ERL_SRC = $(wildcard ./*.erl)

.PHONY: all clean
.SUFFIXes: .erl .beam

all: $(ERL_SRC:.erl=.beam)

%.beam: %.erl
	erlc $<

clean:
	rm -f *.beam
