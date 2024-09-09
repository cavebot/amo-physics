Here is your text converted into Markdown format for better readability:

---

# **bspci2e Package Instructions**

### **1. Setup and Compilation**

1. **Run the script**:
   ```bash
   ./run-bspci2e-simple  
   ```

2. **Ensure the following files are created**:
   - `./lib/libnag.a`
   - `./lib/libslatec.a`

3. **Check the source files in the `./src` directory**:
   - The source files need to be compiled with parameters from:
     - `./src/parameter.1e.inc`
     - `./src/parameter.2e.inc`
   - These files can also be found in the `./run/he/inp` directory.

4. **Produce the executables**:
   - Clean the directory and create the library:
     ```bash
     cd ./src
     make clean cleanlib
     make lib
     ```
   - Generate the executables:
     ```bash
     make fxd_exe    # (generates h1e, h2e, atomic structure executables)
     make tdse_exe   # (if TDSE calculation is to be performed)
     ```
   - The executables should now be in the `./bin` directory.

5. **Check that the script `run-h1e.scr` is in the `./bin` directory**:
   ```bash
   ./bin/run-h1e.scr
   ```

6. **Create or navigate to the `../run/he/` directory**:
   - Create a soft link to the `../../bin` directory:
     ```bash
     cd ./run/he
     ln -s ../../bin
     ```

7. **Directory Tree Structure**:
   ```
   he/
   ├── bin/     # (executables, soft link to ../../bin)
   ├── inp/     # (atomic structure input files: h1e.inp, cfg-0.inp, cfg-1.inp, r12.inp)
   ├── dat/     # (atomic structure data files)
   ├── out/     # (atomic structure output files - ASCII)
   ├── tinp/    # (TDSE input files: tinp/pulse.inp, tinp/tdse_bs.inp)
   ├── tdat/    # (TDSE data files)
   ├── tout/    # (TDSE output files - ASCII)
   └── log/     # (log files)
   ```

### **2. Execution Commands**

1. **1-electron structure and dipole matrix elements (`h1e`)**:
   - Diagonalize the `h1e` Hamiltonian and calculate dipole matrix elements for `l=0,1,..,7`:
     ```bash
     bin/run-h1e.scr 0 7 bin
     ```

2. **2-electron structure and dipole matrix elements (`h2e`)**:

   - Calculate `1/r_12` matrix elements for the `h2e` with `L = 0, 1, 2, 3`:
     ```bash
     bin/Rv2eb 0 > out/v2eb-L0.out
     bin/Rv2eb 1 > out/v2eb-L1.out
     bin/Rv2eb 2 > out/v2eb-L2.out
     bin/Rv2eb 3 > out/v2eb-L3.out
     ```

   - Diagonalize the `h2e` Hamiltonian for `L = 0, 1, 2, 3`:
     ```bash
     bin/Rh2eb 0 > out/h2eb-L0.out
     bin/Rh2eb 1 > out/h2eb-L1.out
     bin/Rh2eb 2 > out/h2eb-L2.out
     bin/Rh2eb 3 > out/h2eb-L3.out
     ```

   - Select the 1000 lower-energy states from the `h2e` diagonalization:
     ```bash
     bin/Rw2eb 0 1500 > out/w2eb-0-1500.out
     bin/Rw2eb 1 1500 > out/w2eb-1-1500.out
     bin/Rw2eb 2 1500 > out/w2eb-2-1500.out
     bin/Rw2eb 3 1500 > out/w2eb-3-1500.out
     ```

   - Calculate 2-electron dipole matrix elements (`Rd2eb`):
     ```bash
     bin/Rd2eb 0 v he > out/d2eb-01-v.out
     bin/Rd2eb 1 v he > out/d2eb-12-v.out 
     bin/Rd2eb 2 v he > out/d2eb-23-v.out 
     ```

     - (Optional, length gauge):
       ```bash
       bin/Rd2eb 0 l he > out/d2eb-01-l.out
       bin/Rd2eb 1 l he > out/d2eb-12-l.out
       ```

   - Convert dipole matrix elements to `dmx` files in netCDF format:
     ```bash
     bin/Rncf 0 3 v 0
     ```
     - (Optional, if length-gauge dipole matrix elements have been calculated):
       ```bash
       bin/Rncf 0 2 l 0
       ```

3. **TDSE Calculation**:
   - If performing TDSE calculations (reads from `tinp/tdse_bs.inp` and `tinp/field.inp`):
     ```bash
     bin/Rtdse_bs
     ```

### **3. Readme from Lampros's DOC Folder**

#### **Compilation Requirements**:

- **Slatec**: 
  - Expand `libslatec.tgs` and create the library:
    ```bash
    gfortran -O2 -c *.f
    ar cst libslatec.a *.o
    ```
  
- **Nag17**: 
  - Expand `libnag.tgz` and create the library:
    ```bash
    ./compile
    ./buildlib
    ```

- **Lapack**:
  ```bash
  sudo apt-get install liblapack-dev
  ```

- **NetCDF Fortran Interface**:
  ```bash
  sudo apt-get install libnetcdff-dev
  ```

#### **Directory Tree**:
```
main (bspci2e)  # (top directory)
├── src         # (location of all the source files and compilation config files)
├── lib         # (package libraries)
├── bin         # (binaries)
└── run         # (execution directories)
    ├── inp     # (input files)
    ├── scr     # (script files)
    ├── dat     # (structure data)
    ├── tdat    # (TDSE data)
    ├── out     # (output files)
    ├── tout    # (TDSE output files)
    └── log     # (log files)
```

---

This Markdown format should make the document more organized and easier to read. You can copy and paste it directly into your Markdown file.