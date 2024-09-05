# He-Simple

## 1. Running the Simulation

To run the simulation, execute the following command:

```bash
./run-bspci2e-simple
```
### Root Directory: `./`

Ensure that the following files have been created:
- `./lib/libnag.a`
- `./lib/libslatec.a`

## 2. Compilation and Setup

1. **Check Source Files:**  
   The source files are located in the `./src` directory. They need to be compiled with the parameters included in:
   - `./src/parameter.1e.inc`
   - `./src/parameter.2e.inc`

   You can also find these files in the `./run/he/inp` directory.

2. **Produce Executables:**  
   Follow these steps to create the executables:

   ```bash
   cd ./src
   make clean cleanlib
   make lib
   make fxd_exe     # Generates h1e, h2e, and atomic structure executables
   make tdse_exe    # If tdse calculation is needed
   ```

   The executables will be placed in the `./bin` directory.

3. **Check Script Placement:**  
   Ensure that the `run-h1e.scr` script is placed in the `./bin` directory:

   ```bash
   ./bin/run-h1e.scr
   ```

4. **Link Executables:**  
   Go to (or create) the `../run/he/` directory. Then, make a soft link to the executables in the `../../bin` directory:

   ```bash
   cd ./run/he
   ln -s ../../bin
   ```

## 3. File Tree Structure

After setting up, your file tree should look like this:

```
he/
   ├── bin/      # Executables (soft link to ../../bin)
   ├── inp/      # Atomic structure input files: h1e.inp, cfg-0.inp, cfg-1.inp, r12.inp
   ├── dat/      # Atomic structure data files
   ├── out/      # Atomic structure output files (ASCII)
   ├── tinp/     # TDSE input files: pulse.inp, tdse_bs.inp
   ├── tdat/     # TDSE data files
   ├── tout/     # TDSE output files (ASCII)
   └── log/      # Log files
```

## 4. Running the 1-Electron Calculation

For hydrogenic targets, the Hamiltonian is given by:

```
h1e = V^2/2 + V(r), where V(r) = -Z/r for hydrogenic targets
```

To diagonalize the 1-electron Hamiltonian and calculate dipole matrix elements for `l = 0,1,...,7`, use the following command:

```bash
bin/run-h1e.scr 0 7 bin
```

## 5. Running the 2-Electron Calculation

The 2-electron Hamiltonian is given by:

```
h2e = h1e(1) + h1e(2) + 1/r_12
```

### Steps for the 2-Electron Calculation:

1. **Calculate `1/r_12` Matrix Elements:**

   The configurations in `cfg-0.inp`, `cfg-1.inp`, and `cfg-2.inp` determine the total number of 2-electron energy states, `NH2E(L)`, for each `L`. To calculate `1/r_12` matrix elements for `L = 0, 1, 2, 3`:

   ```bash
   bin/Rv2eb 0 > out/v2eb-L0.out
   bin/Rv2eb 1 > out/v2eb-L1.out
   bin/Rv2eb 2 > out/v2eb-L2.out
   bin/Rv2eb 3 > out/v2eb-L3.out
   ```

2. **Diagonalize the 2-Electron Hamiltonian:**

   To diagonalize the Hamiltonian and solve for energy states for `L = 0, 1, 2, 3`:

   ```bash
   bin/Rh2eb 0 > out/h2eb-L0.out
   bin/Rh2eb 1 > out/h2eb-L1.out
   bin/Rh2eb 2 > out/h2eb-L2.out
   bin/Rh2eb 3 > out/h2eb-L3.out
   ```

3. **Select Lower-Energy States:**

   Select the lower-energy states (`NW2E = 1000`) from the diagonalized states. `NW2E(L) < NH2E(L)`:

   ```bash
   bin/Rw2eb 0 1500 > out/w2eb-0-1500.out
   bin/Rw2eb 1 1500 > out/w2eb-1-1500.out
   bin/Rw2eb 2 1500 > out/w2eb-2-1500.out
   bin/Rw2eb 3 1500 > out/w2eb-3-1500.out
   ```

4. **Calculate 2-Electron Dipole Matrix Elements:**

   Calculate the dipole matrix elements for `0 -> 1` and `1 -> 2`:

   ```bash
   bin/Rd2eb 0 v he > out/d2eb-01-v.out
   bin/Rd2eb 1 v he > out/d2eb-12-v.out
   bin/Rd2eb 2 v he > out/d2eb-23-v.out
   ```

   Optionally, for the length gauge (depending on your problem):

   ```bash
   bin/Rd2eb 0 l he > out/d2eb-01-l.out
   bin/Rd2eb 1 l he > out/d2eb-12-l.out
   ```

5. **Generate `dmx` Files in NetCDF Format:**

   This step reads from the calculated `dmx` files in the `dat/` directory:

   ```bash
   bin/Rncf 0 3 v 0
   ```

   Optionally, if the length-gauge dipole matrix elements were calculated earlier:

   ```bash
   bin/Rncf 0 2 l 0
   ```

## 6. TDSE Calculation

If TDSE calculation is required, it reads from `tinp/tdse_bs.inp` and `tinp/field.inp`. You can run it with:

```bash
bin/Rtdse_bs
```

This completes the workflow for running the He-Simple simulation.
```