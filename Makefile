# Basic Makefile for Fortran 90/95 projects where modules are
# locate in $(MOD_DIR) directory, procedures in $(SRC_DIR)
# and the main program is $(PROGRAM)
# The .o and .mod are created in $(OBJ_DIR) directory

# Basename of main program without extention (assumed .f90)
PROGRAM:=
MOD_DIR:=modules
SRC_DIR:=
OBJ_DIR:=objects

# Compiler
FC:=gfortran
# Options for shared library
LDFLAGS:=
# Compiler options
# Leave -J$(MOD_DIR) as .mod are put here
FCFLAGS:=-J$(OBJ_DIR) -g -fbacktrace -Wall -pedantic -O2 -fcheck=all

# Getting the dependencies of the modules
MOD_WITH_DEPENDS :=$(shell ./make_depends_tree.sh $(MOD_DIR))
# Getting the .o file name from module source code
MOD_OBJECTS := $(patsubst $(MOD_DIR)/%,$(OBJ_DIR)/%,	\
								$(patsubst %.f90,%.o,$(MOD_WITH_DEPENDS)))

# Same for standalone procedure
OBJECTS := $(patsubst $(SRC_DIR)/%,$(OBJ_DIR)/%, \
						$(patsubst %.f90,%.o,$(wildcard $(SRC_DIR)/*.f90)) )

.PHONY: clean all info depend

# Default rule
# Display info, create $(OBJ_DIR),
# make dependancies and the main program
all: info $(OBJ_DIR) depend $(PROGRAM).exe

# Display some info
info:
	@echo "make: Compiler options:"
	@echo "make: $(FCFLAGS)"

# Rule for the $(OBJ_DIR) directory
$(OBJ_DIR):
	@if [ ! -d $(OBJ_DIR) ]; then \
	echo 'make: Creating $(OBJ_DIR)/'; \
	mkdir $(OBJ_DIR); fi

# Compiling the modules with respect their dependancy
depend: $(MOD_OBJECTS)
	@echo 'make: Dependancies done'

# Rule for module files
$(OBJ_DIR)/%.o: $(MOD_DIR)/%.f90
	@echo 'make: Compiling the module' $<
	@$(FC) $(FCFLAGS) -o $@ -c $<

# Rule for sttandalone procedure
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.f90
	@echo 'make: Compiling the procedure' $<
	@$(FC) $(FCFLAGS) -o $@ -c $<

# Rule for the main program
$(OBJ_DIR)/%.o: %.f90
	@echo 'make: Compiling the main program' $<
	@$(FC) $(LDFLAGS) $(FCFLAGS) -o $@ -c $<

# Génération de l'exécutable
$(PROGRAM).exe: $(OBJ_DIR)/$(PROGRAM).o $(OBJECTS) $(MOD_OBJECTS)
	@echo 'make: Linking objects'
	@$(FC) $(FCFLAGS) -o $@ $^
	@echo 'make: Program '$@' successfuly created.'

clean:
	@echo 'make: rm -rf $(PROGRAM).exe $(OBJ_DIR)'
	@rm -rf $(PROGRAM).exe $(OBJ_DIR)