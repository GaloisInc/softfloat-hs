# SYSTEM=Linux-386-GCC
# SYSTEM=Linux-386-SSE2-GCC
# SYSTEM=Linux-ARM-VFPv2-GCC
SYSTEM=Linux-x86_64-GCC
# SYSTEM=Win32-MinGW
# SYSTEM=Win32-SSE2-MinGW
# SYSTEM=Win64-MinGW-w64
# SYSTEM=template-FAST_INT64
# SYSTEM=template-not-FAST_INT64

COMPILE_TYPE=8086-SSE
# COMPILE_TYPE=8086
# COMPILE_TYPE=ARM-VFPv2-defaultNaN
# COMPILE_TYPE=ARM-VFPv2
# COMPILE_TYPE=RISCV

UNAME := $(shell uname)

# COMPILE_OPTS="\"

ifeq ($(UNAME), Linux)
SPECIALIZE_TYPE=$(COMPILE_TYPE) SOFTFLOAT_OPTS="-DSOFTFLOAT_ROUND_ODD -DINLINE_LEVEL=5 -DSOFTFLOAT_FAST_DIV32TO16 -DSOFTFLOAT_FAST_DIV64TO32 -fpic"
CFLAGS = -shared
LIBPATH=/usr/lib/libsoftfloat.so
endif
ifeq ($(UNAME), Darwin)
SPECIALIZE_TYPE=$(COMPILE_TYPE) # SOFTFLOAT_OPTS="-DSOFTFLOAT_ROUND_ODD -DINLINE_LEVEL=5 -DSOFTFLOAT_FAST_DIV32TO16 -DSOFTFLOAT_FAST_DIV64TO32"
CFLAGS = -dynamiclib
LIBPATH=/usr/local/lib/libsoftfloat.dylib
endif

SOFTFLOAT_PATH = berkeley-softfloat-3/build/$(SYSTEM)
TESTFLOAT_PATH = berkeley-testfloat-3/build/$(SYSTEM)

all: softfloat testfloat fenv

fenv:
	cd test/fenv && make

uninstall: clean
	sudo rm $(LIBPATH)

install:
	sudo cp lib/libsoftfloat.so $(LIBPATH)

softfloat:
	cd $(SOFTFLOAT_PATH) &&	make SPECIALIZE_TYPE=$(SPECIALIZE_TYPE)
	mkdir -p lib
	gcc $(CFLAGS) -o lib/libsoftfloat.so $(SOFTFLOAT_PATH)/*.o

testfloat:
	cd $(TESTFLOAT_PATH) &&	make all
	ln -s ../$(TESTFLOAT_PATH)/testfloat lib/testfloat
	ln -s ../$(TESTFLOAT_PATH)/testfloat_gen lib/testfloat_gen
	ln -s ../$(TESTFLOAT_PATH)/testfloat_ver lib/testfloat_ver
	ln -s ../$(TESTFLOAT_PATH)/testsoftfloat lib/testsoftfloat
	ln -s ../$(TESTFLOAT_PATH)/timesoftfloat lib/timesoftfloat

clean-softfloat:
	cd $(SOFTFLOAT_PATH) && make clean
	rm -rf lib/libsoftfloat.so

clean-testfloat:
	cd $(TESTFLOAT_PATH) && make clean
	rm -rf lib/t*

clean: clean-softfloat clean-testfloat
