softfloat-hs - Haskell bindings for SoftFloat.
===

copyright (c) Ben Selfridge, Galois Inc. 2018

This library consists primarily of a Haskell interface to John Hauser's SoftFloat
library (http://www.jhauser.us/arithmetic/SoftFloat.html). It provides a *pure*
interface to the functions in that library; while the C library is actually impure,
reading and writing to global variables (rounding mode, exceptions), the Haskell
functions have a pure interface, and those variables that were global in the original
library are captured as input arguments and additional outputs of each function.

Installation
===

This library assumes that you have softfloat installed at the following locations:

/usr/local/include/softfloat.h
/usr/local/include/softfloat_types.h
/usr/local/lib/libsoftfloat.a

Once those have been installed, "stack build" should work. Feel free to create an
issue on github if you have any trouble.

Requirements
===

The following are a list of mandatory and secondary requirements for softfloat-hs.

Mandatory Requirements
===

- Must provide a "pure" Haskell interface to all of the SoftFloat functions.
- Must make explicit ALL global variables involved.

Secondary Requirements
===

- Support 80-bit and 128-bit operations.

Current Status
===

The library is functional, although a few global variables are not yet captured
(whether underflow is detected before or after rounding, for example).

Other information
===

* contact: benselfridge@galois.com
