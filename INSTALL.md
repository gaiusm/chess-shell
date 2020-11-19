Fetch the source code
=====================


to obtain and build the source code on a GNU/Linux system, perform the
following:

Building the code
=================

git clone https://github.com/gaiusm/chess-shell
mkdir build-chess
cd build-chess
../chess-shell/configure
make


Run chess shell with the GUI
============================

python3 ../chess-shell/chessPieces.py ../chess-shell/examples/rooks.cs


Run regression tests
====================

make check


Install chess shell
===================

make install
