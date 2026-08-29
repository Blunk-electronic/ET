HOW TO BUILD ET
===============

1. via the classical make
-------------------------
This is only intended to test subprograms in
non-graphical mode. See the Makefile for configuration.

Run in this directory:

make 

or to speed up things on a two-core or four-core machine:

make -j2

or

make -j4

Object files are generated and placed in this directory.
The executable is placed in $HOME/bin.

To clean up run:

make clean


2. via gprbuild
---------------

Build with command:

gprbuild

or to speed up things on a two-core or four-core machine:

gprbuild -j2

or 

gprbuild -j4

Object files are generated and placed in directory obj.
The executable is placed in $HOME/bin.

To clean up run command:

gprclean
