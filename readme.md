# BSPCI2-e Package

### Compilation:
**slatec**  
  Expand `libslatec.tgs` and create the library using the following commands:  
  `gfortran -O2 -c *.f`  
  `ar cst libslatec.a *.o`
  
**nag17**  
  (Expand `libnag.tgz` and create the library using:  
  `./compile`  
  `./buildlib`)
  
**Lapack and Netcdff**  
  Install these libraries in your terminal via the following commands:  
  
```
$ sudo apt-get install liblapack-dev
$ sudo apt-get install libnetcdff-dev
$ sudo apt install libopenblas-dev ??????? IS this needed for paralellism? Ask Andrew Forembski 
```

---

## Directory Tree Structure

```plaintext
main (bspci2e)   # Top directory
├── src          # All source files and compilation configuration files
├── lib          # Libraries not managed by package managers such as apt/brew are located here
├── bin          # Executables
└── run          # Results of runs for different run configurations are here and sorted as below 
    │
    ├── helium           # Runs for Helium simulations go here
    │   └── n5998-l4     # Runs for different atomic basis' get their own folder (manually created)
    │       ├── inp      # Input files
    │       ├── scr      # Script files
    │       ├── dat      # Structure data
    │       ├── tdat     # TDSE data
    │       ├── out      # Output files
    │       ├── tout     # TDSE output files
    │       └── log      # Log and variable files
    │
    └── qdot             # Runs for Qdot simulations go here
        └── n5998-l4     
            ├── inp      
            ├── scr      
            ├── dat      
            ├── tdat     
            ├── out      
            ├── tout     
            └── log      





