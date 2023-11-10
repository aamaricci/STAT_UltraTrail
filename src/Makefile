EXE=analyze_time_series.f90
FC=mpif90
PLAT=gnu
DIREXE=$(HOME)/.bin

define colorecho	
	@tput setaf 6
	@tput bold
	@echo $1
	@tput sgr0
endef





#NO NEED TO CHANGE DOWN HERE, only expert mode.
#########################################################################
GLOB_INC:=$(shell pkg-config --cflags scifor)
GLOB_LIB:=$(shell pkg-config --libs scifor)

ifeq ($(PLAT),intel)
FFLAG=-O2 -ftz
OFLAG=-O3 -ftz
DFLAG=-p -O0 -g -fpe0 -warn -warn errors -debug extended -traceback -check all,noarg_temp_created
FPPSERIAL =-fpp -D_
FPPMPI =-fpp -D_	
endif
ifeq ($(PLAT),gnu)
FFLAG = -O2 -ffree-line-length-none
DFLAG = -O2 -p -g -fimplicit-none -Wsurprising -Wall -Waliasing -fwhole-file -fcheck=all -pedantic -fbacktrace -ffree-line-length-none
OFLAG = -O3 -ffast-math -march=native -funroll-loops -ffree-line-length-none
FPPSERIAL =-cpp -D_
FPPMPI =-cpp -D_MPI	
endif

OBJS= DATA_TUPLE.o LIST_TUPLE.o

##$ REVISION SOFTWARE VARIABLES
REV=$(shell git rev-parse HEAD)
VER = 'character(len=41),parameter :: revision = "$(REV)"' > revision.inc

EXEC=$(shell basename -s .f90 ${EXE})

##$ Extends the implicit support of the Makefile to .f90 files
.SUFFIXES: .f90

all: FLAG:=${FFLAG} ${FPPSERIAL}
all: ${OBJS}
	@echo ""
	$(call colorecho,"compiling $(EXEC).f90 ")
	@echo ""
	$(FC) $(OBJS) $(FLAG) $(EXEC).f90 -o $(DIREXE)/$(EXEC) ${GLOB_INC} ${GLOB_LIB}
	@echo "Done"

debug: FLAG:=${DFLAG} ${FPPSERIAL}
debug: ${OBJS}
	@echo ""
	$(call colorecho,"compiling $(EXEC).f90 ")
	@echo ""
	$(FC) $(OBJS) $(FLAG) $(EXEC).f90 -o $(DIREXE)/$(EXEC) ${GLOB_INC} ${GLOB_LIB}
	@echo "Done"


.f90.o:	
	$(FC) $(FLAG) -c $< ${GLOB_INC}

clean: 
	@echo "Cleaning:"
	@rm -f *.mod *.o *~
	@rm -fv  $(DIREXE)/$(EXEC)



#########################################################################
