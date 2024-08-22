# Set up the project directory
PROJECT_DIR = /home/ttrw2/Source/FunctionPassingFortran
#PROJECT_DIR = /home/raw54/Source/OptimusPrime

# Where to hide things
BIN_DIR = $(PROJECT_DIR)/bin
SRC_DIR = $(PROJECT_DIR)/src
LIB_DIR = $(PROJECT_DIR)/lib
INC_DIR = $(PROJECT_DIR)/include

# Set up the compiler
#CC=mpicc
#FC=mpifort

#CC=icc
#FC=ifort

CC=gcc
FC=gfortran

# Set the optimisation and error checking options
#OPTIMISE =  -O3 
#DEBUGGING = -pg -O0 -CB -warn all -check all -gen-interfaces -check bounds -ftrapuv -debug full -fpe0 -traceback
DEBUGGING = -g3 -O0 -fcheck=all -ffpe-trap=invalid,zero,overflow -finit-integer=-2147483647 -finit-real=snan -fbacktrace -pedantic -Wall -fsignaling-nans -frounding-math -ffree-line-length-none
INCLUDE = -I$(INC_DIR)

# Bundle these together
FCFLAGS= $(OPTIMISE) $(DEBUGGING) $(INCLUDE)
CCFLAGS= $(OPTIMISE) $(DEBUGGING) $(INCLUDE)

# Set up any libraries needed
LFLAGS= -L$(LIB_DIR) 

# Set up the list of source code files
EXEC_FILES = $(SRC_DIR)/precision.f90 \
             $(SRC_DIR)/functional/stochastic.f90 \
             $(SRC_DIR)/functional/ranking.f90 \
             $(SRC_DIR)/functional/interface.f90 \
             $(SRC_DIR)/userfunctions.f90 \
             $(SRC_DIR)/optimisers/sa.f90 \
             $(SRC_DIR)/optimisers/ga.f90 \
             $(SRC_DIR)/optimisers/es.f90 \
             $(SRC_DIR)/optimisers.f90 \
             $(SRC_DIR)/main.f90 

# Set up the list of object files from the source code files
OBJ_FILES = $(EXEC_FILES:%.f90=%.o)

# Set up the hooks
all: clean runOptimus.exe

runOptimus.exe: $(OBJ_FILES)
	@echo ""
	@echo "Building Optimus Prime testing executable"
	$(FC) $(FCFLAGS) $(OBJ_FILES) -o $@ $(LFLAGS)
	$(MKDIR) $(BIN_DIR)
	$(MV) $@ $(BIN_DIR)/$@

%.o: %.F90
	@echo ""
	@echo "Compiling F90 file " $*.F90
	$(FC) -c $(FCFLAGS)  -o $@ -c $<

%.o: %.f90
	@echo ""
	@echo "Compiling f90 file " $*.f90
	$(FC) -c $(FCFLAGS)  -o $@ -c $<

%.o: %.f
	@echo ""
	@echo "Compiling f77 file " $*.f
	$(FC) -c $(FCFLAGS)  -o $@ -c $<

%.o: %.c
	@echo ""
	@echo "Compiling C file " $*.c
	$(CC) -c $(CCFLAGS) -o $@ -c $<

clean:
	$(RM) *.o *.mod *~ *# fort.*
	${RM} $(SRC_DIR)/*.o $(SRC_DIR)/*~ $(SRC_DIR)/*.mod $(SRC_DIR)/*#
	${RM} $(SRC_DIR)/optimisers/*.o $(SRC_DIR)/optimisers/*~ $(SRC_DIR)/optimisers/*.mod $(SRC_DIR)/optimisers/*#
	${RM} $(SRC_DIR)/functional/*.o $(SRC_DIR)/functional/*~ $(SRC_DIR)/functional/*.mod $(SRC_DIR)/functional/*#
	${RM} ${INC_DIR}/*.mod

cleanall:
	$(RM) *.o *.mod *~ *# fort.*
	${RM} $(SRC_DIR)/*.o $(SRC_DIR)/*~ $(SRC_DIR)/*.mod $(SRC_DIR)/*#
	${RM} $(SRC_DIR)/optimisers/*.o $(SRC_DIR)/optimisers/*~ $(SRC_DIR)/optimisers/*.mod $(SRC_DIR)/optimisers/*#
	${RM} $(SRC_DIR)/functional/*.o $(SRC_DIR)/functional/*~ $(SRC_DIR)/functional/*.mod $(SRC_DIR)/functional/*#
	${RM} ${INC_DIR}/*.mod
	${RM} ${BIN_DIR}/*.exe

tidy:
	$(RM) machinefile_* slurm-*.out

new:
	$(RM) bestPerformers.dat historyFile.dat restartFile.dat

#
# Utilities
#

CD     = cd
MV     = mv
RM     = \rm -f
CP     = \cp -f
RMDIR  = \rm -rf
MKDIR  = \mkdir -p
AR     = \ar -crs
