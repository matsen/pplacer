# Just for testing
.PHONY: release debug clean

CXXFLAGS = -std=c++0x -Wall -pedantic -Wextra
LDLIBS = -lbpp-core -lbpp-seq -lbpp-phyl
LDFLAGS = $(CXXFLAGS)

OBJ = lcfit_tripod_testdata.o
EXE = lcfit_tripod_testdata

debug: CXXFLAGS += -g -O0
debug: LDFLAGS += -g -O0
debug: $(EXE)

release: CXXFLAGS += -O3
debug: LDFLAGS += -g -O3
release: $(EXE)

$(EXE): $(OBJ)

clean:
	rm -f $(OBJ) $(EXE)
