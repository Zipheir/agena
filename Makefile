OBJ = agena.o
BIN = $(OBJ:.o=)

# Debug build.
CSCFLAGS = -O0 -d2 -v -local

# Normal build.
#CSCFLAGS = -O2 -local

.SUFFIXES: .scm

all: agena

.scm:
	csc $(CSCFLAGS) -o $@ $<

clean:
	rm -f $(BIN) $(OBJ)
