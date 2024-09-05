#He- simple
#
#   ./run-bspci2e-simple  
#
#    root directory: ./
#
# --    ensure that   './lib/libnag.a' , './lib/libslatec.a' have created 
#
#
# --    check  in './src' 
#       the source files need to be compiled with the parameters included in:
#       edit according the problem: 
#
#       './src/parameter.1e.inc' './src/parameter.2e.inc'
#
#       (they can also be found in the './run/he/inp' directory
#       
#      -- produce the executables (placed in './bin' directory)
#
#
#         ./src> make clean cleanlib
#         ./src> make lib 
#         ./src> make fxd_exe     (generates h1e, h2e,  atomic structure executables)
#         ./src> make tdse_exe    (if tdse calculation is to be performed)
#
#         now executables should be in './bin' directory 
#
#      -- check that the script 'run-h1e.scr' is placed in the './bin' directory:   
#
#        ./bin/run-h1e.scr 
# 
#            
#     -- go to (or create the)  ../run/he/ directory 
#         
#          -- make a soft link to the executables '../../bin' directory
#  
#        ./run/he> ln -s ../../bin
#
#         --file tree structure:
#     
#            he/
#               -- bin/     (executables)  (soft link to ../../bin)
#               -- inp/     (atomic structure input files: h1e.inp, cfg-0.inp, cfg-1.inp, r12.inp 
#               -- dat/     (atomic structure all data files stored) 
#               -- out/     (atomic structure output files   (ascii)
#
#               -- tinp/     (tdse input files: tinp/pulse.inp tinp/tdse_bs.inp 
#               -- tdat/     (tdse data files)
#               -- tout/     (tdse out files (ascii)
# 
#                -- log/     (log files)
# 

#bin/Rbspci2e -v 0.0 -zn 2.0 -rb 300 -nb 602

#
#  h1e = V^2/2 + V(r),     for hydrogenic targets V(r) = -Z/r 
#
# 1-electron structure and dipole matrix elements

# Diagonalize h1e Hamiltonian and calculate dipole matrix elements for l=0,1,..,7
#
#Rh1e:    h1e|nl> = e_nl |nl>
#
#Rw1e:     dz = <nl|d|n'l+1 > for d = length, velocity gauge 
#

bin/run-h1e.scr 0 7 bin

#
# 2-electron structure and dipole matrix elements
#
#         h2e = h1e(1) + h1e(2) + 1/r_12
#
# 

# Rv2eb:
# Calculate 1/r_12 matrix elements for the h2e L =0, 1, 2
#
# the configurations included in the cfg-0.inp, cfg-1.inp, cfg-2.inp
# determine the total number of 2e-energy states, NH2E(L) for each L
#


bin/Rv2eb 0     > out/v2eb-L0.out
bin/Rv2eb 1     > out/v2eb-L1.out
bin/Rv2eb 2     > out/v2eb-L2.out
bin/Rv2eb 3     > out/v2eb-L3.out

#Rh2eb:
# Diagonalize h2e Hamiltonian 
# h2e |NL> = E_NL |NL>  L =0,1,2, n,  NH2E(L): total 2e-energy states for L

bin/Rh2eb 0     > out/h2eb-L0.out
bin/Rh2eb 1     > out/h2eb-L1.out
bin/Rh2eb 2     > out/h2eb-L2.out
bin/Rh2eb 3     > out/h2eb-L3.out

#Rw2eb:
# select the NW2E = 1000 lower-energy states from the h2e diagonalization
# NW2E(L) < NH2E(L)   
#

bin/Rw2eb 0 1500   > out/w2eb-0-1500.out
bin/Rw2eb 1 1500   > out/w2eb-1-1500.out
bin/Rw2eb 2 1500   > out/w2eb-2-1500.out
bin/Rw2eb 3 1500   > out/w2eb-3-1500.out

# Rd2eb
# 2e-dipole matrix elements 0->1 and 1->2 
#  < N L | (z_1 + z_2 | N', L+1>,   N =1,..,NW2E < NHX
#
# dipole matrix elements are stored in dat/
bin/Rd2eb 0 v he > out/d2eb-01-v.out
bin/Rd2eb 1 v he > out/d2eb-12-v.out 
bin/Rd2eb 2 v he > out/d2eb-23-v.out 

# in the length gauge (optional,depending on the problem)
#bin/Rd2eb 0 l he > out/d2eb-01-l.out
#bin/Rd2eb 1 l he > out/d2eb-12-l.out 



#  dmx files in netCDF format (reads from the calculated dmx files in dat/ )
bin/Rncf 0 3 v 0

# optional if the legnth-gauge dipole matrix elements have calculated earlier)
#bin/Rncf 0 2 l 0

#.... tdse  (reads tinp/tdse_bs.inp, tinp/field.inp)
#bin/Rtdse_bs


contents of other readme from lampros's DOC folder 
##############
-bspci2e package

20112017:
=====================================================
compilation requires:
-slatec          (expand libslatec.tgs and create library as: gfortran -O2 -c *.f ; ar cst libslatec.a *.o)
-nag17           (expand libnag.tgz  and create library as:  ./compile; ./buildlib)
-lapack          (sudo apt-get install liblapack-dev)
-netcdff         (sudo apt-get install libnetcdff-dev)
======================================================


directory tree

main (bspci2e)     (top directory)
-src
-bin
-run
-lib


src: the location of all the source files and compilation config files
lib: packages lib are located here
run: execution dirs
 -inp : input files
 -scr : script files
 -dat : structure data
 -tdat: tdse dat
 -out : out files
 -tout: TDSE out files
 -log: var
==========================================================

