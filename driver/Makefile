# Name of the executable
PROG = Test_Sort.x

# Choose compiler : GNU or INTEL
#COMPILER = GNU
COMPILER = INTEL

# Fortran compiler options
IFFLAGS = -O
GFFLAGS = -O

# Location of Src and Obj directories
SRC_DIR = ./
OBJ_DIR = ./

# Complier command with common flags
IFC = ifort    -module $(OBJ_DIR) -fpp $(PREPROC)
GFC = gfortran -J$(OBJ_DIR)       -cpp $(PREPROC)

# Check which compiler is being used and choose appropriate flags
ifeq ($(COMPILER),GNU)
  FC = $(GFC)
  FFLAGS = $(GFFLAGS)
  MKL = $(GMKL)
  ARPACK = $(GARPACK)
else ifeq ($(COMPILER),INTEL)
  FC = $(IFC)
  FFLAGS = $(IFFLAGS)
  MKL = $(IMKL)
  ARPACK = $(IARPACK)
else
  FC =
  FFLAGS =
  MKL =
  ARPACK =
endif

# Source files

SRCS += \
$(SRC_DIR)/Test_Sort.f90

VPATH := $(dir $(SRCS))

OBJS := $(patsubst %.f90, $(OBJ_DIR)/%.o, $(notdir $(SRCS)))

LIBS = $(ARPACK) $(MKL) $(TECIO)

# Rules for compilation
all: $(PROG)

$(PROG): $(OBJS)
	@$(FC) $(FFLAGS) -o $@ $(OBJS) $(LIBS)
	@echo '-------------------------------'
	@echo ' the build completed           '
	@echo '-------------------------------'

clean:
	@rm -f $(PROG) $(OBJ_DIR)/*.o $(OBJ_DIR)/*.mod

# Each subdirectory must supply rules for building sources it contributes
$(OBJ_DIR)/%.o: %.f90 
	$(FC) $(FFLAGS) -c $< -o $@

