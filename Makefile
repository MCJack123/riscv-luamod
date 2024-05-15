TRIPLET=riscv32-unknown-linux-gnu-
CC=$(TRIPLET)gcc
AR=$(TRIPLET)ar
RANLIB=$(TRIPLET)ranlib
CFLAGS=

testmod.so: testmod.c lua52.a
	$(CC) -fPIC -shared $(CFLAGS) -o $@ $^

lua52.a: lua52.o lauxlib.o
	rm $@
	$(AR) rcs $@ $^
	$(RANLIB) $@

%.o: %.c
	$(CC) -fPIC $(CFLAGS) -o $@ $<