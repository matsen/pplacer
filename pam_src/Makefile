CFLAGS = -Wall -pedantic -std=c99 -DPAM_TEST
LDLIBS = -lgsl -lgslcblas

debug: CFLAGS += -g -O0
debug: pam

release: CFLAGS += -O3
release: pam

clean:
	rm -f *.o pam

.PHONY: debug release clean
