
---

# **bspci2e Package**

This package is designed to compute atomic structures and dipole matrix elements for 1-electron and 2-electron systems, and to perform time-dependent Schrödinger equation (TDSE) calculations.

---

## **Table of Contents**

1. [Project Structure](#project-structure)
2. [Installation and Library Setup](#installation-and-library-setup)
   - [Dependencies](#dependencies)
   - [Compilation of External Libraries](#compilation-of-external-libraries)
3. [Compilation of Executables](#compilation-of-executables)
4. [Execution Commands](#execution-commands)
   - [1-electron Structure and Dipole Matrix](#1-electron-structure-and-dipole-matrix)
   - [2-electron Structure and Dipole Matrix](#2-electron-structure-and-dipole-matrix)
   - [TDSE Calculations](#tdse-calculations)

---

## **Project Structure**

The directory tree for the bspci2e package is organized as follows:

```bash
bspci2e/               # Main directory
├── src/               # Source code and compilation configuration
├── lib/               # Package libraries (libnag.a, libslatec.a)
├── bin/               # Binaries (executables)
└── run/               # Execution directories
    ├── he/            # Atomic structure and TDSE calculation directory
    │   ├── bin/       # Soft link to ../../bin (executables)
    │   ├── inp/       # Input files for atomic structure (h1e.inp, cfg-0.inp, cfg-1.inp, r12.inp)
    │   ├── dat/       # Atomic structure data files
    │   ├── out/       # Output files (ASCII)
    │   ├── tinp/      # TDSE input files (pulse.inp, tdse_bs.inp)
    │   ├── tdat/      # TDSE data files
    │   ├── tout/      # TDSE output files (ASCII)
    │   └── log/       # Log files
```

---

## **Installation and Library Setup**

### **Dependencies**

Ensure the following libraries are installed on your system:

- **Lapack & openBLAS**:
  ```bash
  sudo apt-get install liblapack-dev
  sudo apt-get install libopenblas-dev
   ```

- **NetCDF Fortran Interface**:
  ```bash
  sudo apt-get install libnetcdff-dev
  ```

### **Compilation of External Libraries**

#### **Slatec Library**

1. Expand the `libslatec.tgs` archive.
2. Compile the Slatec library:
   ```bash
   gfortran -O2 -c *.f
   ar cst libslatec.a *.o
   ```
3. Move the generated library into the `./lib` directory:
   ```bash
   mv libslatec.a ./lib/
   ```

#### **Nag17 Library**

1. Extract the `libnag.tar.gz` archive in ./lib (this should be included in the repo).
2. Compile and build the Nag library:
   ```bash
   cd libnag/scripts
   ./compile
   ./buildlib
   ```
3. Move the generated library into the `./lib` directory:
   ```bash
   mv libnag.a ./lib/
   ```

---

## **Compilation of Executables**

1. **Navigate to the source directory**:
   ```bash
   cd ./src
   ```

2. **Clean previous builds**:
   ```bash
   make clean cleanlib
   ```

3. **Compile the libraries**:
   ```bash
   make lib
   ```

4. **Generate the executables**:
   - For 1-electron and 2-electron atomic structure calculations:
     ```bash
     make fxd_exe
     ```
   - For TDSE calculations:
     ```bash
     make tdse_exe
     ```

   The compiled executables will be placed in the `./bin` directory.

5. **Set up symbolic links for execution**:
   - Navigate to the `run/{quantum_system}/{specific_run_folder}` directory and create a symbolic link to the `bin` folder:
     ```bash
     cd ./run/he/asd/
     ln -s ../../../bin (this command might vary depending on OS and how many levels directories deep into the run folder you are executing your runs from)
     ```

---

## **Execution Commands**

### **1-electron Structure and Dipole Matrix**

To calculate the 1-electron structure and dipole matrix elements (`h1e`):

1. **Diagonalize the `h1e` Hamiltonian and calculate dipole matrix elements** for `l = 0` to `7`:
   ```bash
   bin/run-h1e.scr 0 7 bin
   ```

### **2-electron Structure and Dipole Matrix**

For 2-electron structure and dipole matrix elements (`h2e`):

1. **Calculate `1/r_12` matrix elements** for `h2e` with `L = 0, 1, 2, 3`:
   ```bash
   bin/Rv2eb 0 > out/v2eb-L0.out
   bin/Rv2eb 1 > out/v2eb-L1.out
   bin/Rv2eb 2 > out/v2eb-L2.out
   bin/Rv2eb 3 > out/v2eb-L3.out
   ```

2. **Diagonalize the `h2e` Hamiltonian** for `L = 0, 1, 2, 3`:
   ```bash
   bin/Rh2eb 0 > out/h2eb-L0.out
   bin/Rh2eb 1 > out/h2eb-L1.out
   bin/Rh2eb 2 > out/h2eb-L2.out
   bin/Rh2eb 3 > out/h2eb-L3.out
   ```

3. **Select the 1000 lower-energy states** from the `h2e` diagonalization:
   ```bash
   bin/Rw2eb 0 1500 > out/w2eb-0-1500.out
   bin/Rw2eb 1 1500 > out/w2eb-1-1500.out
   bin/Rw2eb 2 1500 > out/w2eb-2-1500.out
   bin/Rw2eb 3 1500 > out/w2eb-3-1500.out
   ```

4. **Calculate 2-electron dipole matrix elements**:
   ```bash
   bin/Rd2eb 0 v he > out/d2eb-01-v.out
   bin/Rd2eb 1 v he > out/d2eb-12-v.out
   bin/Rd2eb 2 v he > out/d2eb-23-v.out
   ```

   - (Optional, for length gauge):
     ```bash
     bin/Rd2eb 0 l he > out/d2eb-01-l.out
     bin/Rd2eb 1 l he > out/d2eb-12-l.out
     ```

5. **Convert dipole matrix elements to `dmx` files in NetCDF format**:
   ```bash
   bin/Rncf 0 3 v 0
   ```

   - (Optional, if length gauge dipole matrix elements were calculated):
     ```bash
     bin/Rncf 0 2 l 0
     ```

### **TDSE Calculations**

To perform TDSE calculations, ensure you have the correct input files (`tinp/tdse_bs.inp` and `tinp/field.inp`):

1. **Run the TDSE calculation**:
   ```bash
   bin/Rtdse_bs
   ```

--- 

This document provides a complete guide to setting up, compiling, and executing the bspci2e package for atomic structure and TDSE calculations.