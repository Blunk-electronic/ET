HOW TO BUILD ET
===============

1. via the classical make
-------------------------
run in this directory:

make 

or to speed up things on a two-core or four-core machine:

make -j2

or

make -j4

Object files are generated and placed in this directory.
The executable is placed in $HOME/bin.

NOTE: Currently gtkada is not included. So compiling with make builds
ET without GUI.

To clean up run:

make clean


2. via gprbuild
---------------

gprbuild is a great invention. But it does not come with every Linux distro.
You have install gprbuild first.
The "makefile" et.gpr includes the gtkada library. You must also install gtkada.
See <https://github.com/Blunk-electronic/M-1/blob/master/gtkada_install.txt>
Building with gprbuild builds ET with a GUI.

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
