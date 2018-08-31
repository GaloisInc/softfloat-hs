# Script for installing SoftFloat as a dynamic library, in /usr/local/lib.

# SYSTEM=Linux-386-GCC
# SYSTEM=Linux-386-SSE2-GCC
# SYSTEM=Linux-ARM-VFPv2-GCC
SYSTEM=Linux-x86_64-GCC
# SYSTEM=Win32-MinGW
# SYSTEM=Win32-SSE2-MinGW
# SYSTEM=Win64-MinGW-w64
# SYSTEM=template-FAST_INT64
# SYSTEM=template-not-FAST_INT64

# COMPILE_TYPE=8086-SSE
# COMPILE_TYPE=8086
# COMPILE_TYPE=ARM-VFPv2-defaultNaN
# COMPILE_TYPE=ARM-VFPv2
COMPILE_TYPE=RISCV

# COMPILE_OPTS="\"

set -x

cd berkeley-softfloat-3/build/$SYSTEM/
make SPECIALIZE_TYPE=$COMPILE_TYPE SOFTFLOAT_OPTS="-DSOFTFLOAT_ROUND_ODD -DINLINE_LEVEL=5 \
                -DSOFTFLOAT_FAST_DIV32TO16 -DSOFTFLOAT_FAST_DIV64TO32 \
                -fpic"
sudo gcc -shared -o /usr/lib/libsoftfloat.so *.o
sudo cp ../../source/include/softfloat_types.h /usr/include
sudo cp ../../source/include/softfloat.h /usr/include
