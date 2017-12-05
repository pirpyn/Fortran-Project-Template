# FORTRAN Project Template

This git provides a Makefile and a bash script to start a small FORTRAN project

It's aim is to help beginners to set up a simple working project.

## Bash
The make_depends.sh prints modules in such order with respects to their dependencies. It's really simple and isn't intented to clever way to set up depencies.

## Makefile
The makefile provided looks for modules in `MOD_DIR` (modules) directory, other sources in `SRC_DIR` (sources), compiles each file in  `OBJ_DIR` (objects), then generates the `LIB` (lib.a) library with 'ar' in `OBJ_DIR`, then takes `SRC_DIR/PROGRAM.f90` as the main program file and generates `PROGRAM.exe`.

## Modules

It also provides somes modules to help you start; Currently:

modules/mod_primat.f90: declare a routine `printmat` to write in file or the standart output rank 1 or 2 array of type integer, real, complex and logical as they are usually represented. Rank 1 array are drawn on a signe line

modules/mod_io.f90: declare a routine `read_val` to read a file formated in a special way to find a value and return it. Type of the value being either integer, real or complex and is either scalar or rank 1 array.

