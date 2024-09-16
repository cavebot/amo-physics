# 07062013: using acml library for blas/lapack
# linking with openmp acml for h2eb.f90 (dsyevx lapack routine)


# todo:(May/2013)  
#   ~~~~~~~~~~~
#    -- use only one parameter.2e.inc file throughout the package 
#       allow bspci2e.f90 to write bspci2e.inc where the values of nl,np,... 
#       are declared depending on the problem
#    -- incorporate the hartree-fock in *f90 package
#    -- consider bspci1e calculations as special case of bspc2e.pl script
#    -- remove np,kx from bs_parameter.f90 in favor to no,kb in bspci2e.inp
#    -- remove all unused variables 
#    -- declare all functions with explicit interface
#    -- transform v2eb.f to v2eb.f90
#    -- when using loops check for right_to_left execution of loops
#    -- rewrite n2e.f in fortran90 
    
#   compilers:
#   ~~~~~~~~~~~
#   - implement two modes (debugging,optimization)
#   - test code with: gfortran, intel, open64   

#   parallelism:
#   ~~~~~~~~~~~
#   -- explore openmp parallelization in loops (v2eb.f90)
   
#    physics:
#    ^^^^^^^^
#    -- incorporate quantum dot potentials
#    -- reconsider n2e.f90 normalization code
#    -- include other model potentials as well

# Todo
# -- incorporating the 3e code in the Makefile?

# 20102008/ transform of d2e.f (cpc/bspci2e v1 code) to h2eb.f90 (fortran 90)           

# Makefile bspci2e
# local  dirs

CMP=gnu  # Change to intel for intel compiler


DIR    	 = ${shell pwd}
SRC    	 = ${DIR}/src
LIB    	 = ${DIR}/lib
MOD      = ${SRC}
BIN   	 = ${DIR}/bin

TOP      = basis
LIBNAME  = lib${TOP}.a
LINK_LIB = ${LIB}/${LIBNAME} 

# mpi  
MPI	  	   = mpif77
MPI_FC     = mpif77
MPI_LINK   = mpif77

ifeq ($(CMP),intel)
include ./compilers/intel.cfg
else
include ./compilers/gnu.cfg
endif	

##########################################################################################
MOBJECTS  = #${MOD}/mod_pad_utils.o
MSOURCES  = #${MOBJECTS:.o=.f90} 
MPRODUCT  = #mod
##########################################################################################

LOBJECTS  =   mod_precision.o   mod_units.o     mod_io.o     mod_netCDF.o \
               bs_fr_1e_ang.o      anglib.o     mod_spherical_harmonics.o \
               modules_tdse.o   mod_field.o     \
                      spack.o    bsplines.o    soleig-fxd.o     ang.o     \
                       grid.o      rinfxd.o       rinfree.o  cxfin1.o subio.o \
		     ykfct.o       sub_hf.o    mod_netCDF_new.o    
#              sub_hf.o           ykfct.o   #used when the hf will be implemented   
LSOURCES  = ${LOBJECTS:.o=.f}  
LOBJECTS := $(patsubst %, $(SRC)/%, $(LOBJECTS))
LSOURCES := $(patsubst %, $(SRC)/%, $(LSOURCES))
LPRODUCT  = lib

##########################################################################################
POBJECTS1 = bs1e_parameter.o mod_kracken.o bspci2e.o
OBJECTS1  = $(patsubst %, $(SRC)/%, ${POBJECTS1})
SOURCES1  = ${OBJECTS1:.o=.f90}
LIBS1     = 
PRODUCT1  = Rbspci2e
##########################################################################################
# h1e 
##########################################################################################
POBJECTS2 = bs1e_parameter.o mod_utils-v1.o bs1e_modules.o mod_data.o \
            mod_types.o mod_h1e.o sub_hf.o h1e.o
OBJECTS2  = $(patsubst %, $(SRC)/%, ${POBJECTS2})
SOURCES2  = ${OBJECTS2:.o=.f90}
LIBS2     = ${LINK_LIB} ${LINK_LAPACK} ${LINK_NAG17} ${LINK_SLATEC}
PRODUCT2  = Rh1e
##########################################################################################
POBJECTS2b = bsh2p_parameter.o mod_utils-v1.o mod_qutils.o bs1e_modules.o \
             mod_data.o h2p.o
OBJECTS2b = $(patsubst %, $(SRC)/%, ${POBJECTS2b})
SOURCES2b = ${OBJECTS2b:.o=.f90}
LIBS2b    = ${LINK_LIB} ${LINK_LAPACK} ${LINK_NAG17} ${LINK_SLATEC}
PRODUCT2b = Rh2p
##########################################################################################
POBJECTS3 = bs1e_parameter.o mod_utils-v1.o bs1e_modules.o mod_data.o \
            mod_types.o mod_w1e.o w1e.o
OBJECTS3  = $(patsubst %, $(SRC)/%, ${POBJECTS3})
SOURCES3  = ${OBJECTS3:.o=.f90}
LIBS3     = ${LINK_LIB} ${LINK_LAPACK} ${LINK_NAG17} ${LINK_SLATEC}
PRODUCT3  = Rw1e
##########################################################################################
POBJECTS3a = bs1e_parameter.o mod_utils-v1.o bs1e_modules.o mod_data.o \
             mod_types.o mod_w1e_adam.o w1e_adam.o
OBJECTS3a = $(patsubst %, $(SRC)/%, ${POBJECTS3a})
SOURCES3a = ${OBJECTS3a:.o=.f90}
LIBS3a    = ${LINK_LIB} ${LINK_LAPACK} ${LINK_NAG17} ${LINK_SLATEC}
PRODUCT3a = Rw1e_adam
# slatec here is only needed because dbvalu is used in h1e. (Move dbvalue into
# anotherother module than bs1e_modules.f90


##########################################################################################
# h2e fixed (fxd) boundary conditions
##########################################################################################
POBJECTS4 = v2eb.o subiob.o rinfxdb.o ykfctb.o angb.o subv2eb.o
OBJECTS4  = $(patsubst %, $(SRC)/%, ${POBJECTS4})
SOURCES4  = ${OBJECTS4:.o=.f}
LIBS4     = ${LINK_LIB}
PRODUCT4  = Rv2eb
##########################################################################################
POBJECTS4b = v12.o subiov12.o rinfxdb.o ykfctb.o angb.o subv12.o
OBJECTS4b = $(patsubst %, $(SRC)/%, ${POBJECTS4b})
SOURCES4b = ${OBJECTS4b:.o=.f}
LIBS4b    = ${LINK_LIB}
PRODUCT4b = Rv12
##########################################################################################
# 23.10.2008: trying to convert to f90 (not completed)

POBJECTS4a = mod_w2e_r12.o modio.o mod_bs_frb_2e.o r12.o
OBJECTS4a  = $(patsubst %, $(SRC)/%, ${POBJECTS4a})
SOURCES4a  = ${OBJECTS4a:.o=.f90}
LIBS4a     = ${LINK_LIB}
PRODUCT4a  = Rr12b
##########################################################################################
# 20.10.2008: transform old d2e.f (bspci2e/cpc/h2eb.f) from f77 to fortran 90.

POBJECTS5 = mod_w2e.o h2eb.o                 #h2eb_old.o  #subio.o
OBJECTS5  = $(patsubst %, $(SRC)/%, ${POBJECTS5})
SOURCES5  = ${OBJECTS5:.o=.f90}
LIBS5     = ${LINK_LIB} ${LINK_LAPACK} #${LINK_ACML_MP}
PRODUCT5  = Rh2eb
##########################################################################################
#QUB25102008LAAN: transform of old (bspci2e/cpc/wf2e.f) from f77 to fortran 90

POBJECTS6 = w2eb.o                     #subio.o
OBJECTS6  = $(patsubst %, $(SRC)/%, ${POBJECTS6})
SOURCES6  = ${OBJECTS6:.o=.f90}
LIBS6     = ${LINK_LIB}
PRODUCT6  = Rw2eb
##########################################################################################
POBJECTS7 = d2eb.o subd2eb.o         #subio.o
OBJECTS7  = $(patsubst %, $(SRC)/%, ${POBJECTS7})
SOURCES7  = ${OBJECTS7:.o=.f}
LIBS7     = ${LINK_LIB} ${LINK_LAPACK} ${LINK_BLAS}
PRODUCT7  = Rd2eb
##########################################################################################
POBJECTS8 = mod_hnorm_io.o mod_hnorm_wf1e.o mod_hnorm_param.o hnorm.o
OBJECTS8  = $(patsubst %, $(SRC)/%, ${POBJECTS8})
SOURCES8  = ${OBJECTS8:.o=.f90}
LIBS8     = ${LINK_LIB}
PRODUCT8  = Rhnorm
##########################################################################################
#23.10.2008: included in the package
# old names: norm2e.f subnorm2e.
#	 grid.f (needed by norm2e.f) 
          
POBJECTS9 = subio.o rinfxdb.o subn2eb.o n2eb.o #subio.o rinfxdb.o ykfctb.o angb.o subv2eb.o 
OBJECTS9  = $(patsubst %, $(SRC)/%, ${POBJECTS9})
SOURCES9  = ${OBJECTS9:.o=.f} 
LIBS9     = #${LINK_LIB} 
PRODUCT9  = Rn2eb
##########################################################################################
POBJECTS10 = ncf.o  #mod_netCDF.o    #subio.o mod_netCDF.o 
OBJECTS10  = $(patsubst %, $(SRC)/%, ${POBJECTS10})
SOURCES10  = ${OBJECTS10:.o=.f90} 
LIBS10     = ${LINK_LIB} ${LINK_NETCDF_F} 
PRODUCT10  = Rncf
##########################################################################################
# test new mod_netCDF file

POBJECTS10a = ncf_new.o  #mod_netCDF_new.o    #subio.o mod_netCDF.o 
OBJECTS10a  = $(patsubst %, $(SRC)/%, ${POBJECTS10a})
SOURCES10a  = ${OBJECTS10a:.o=.f90} 
LIBS10a     = ${LINK_LIB} ${LINK_NETCDF_F} 
PRODUCT10a  = Rncf_new
##########################################################################################


##########################################################################################
# h2e free-boundary conditions
##########################################################################################

POBJECTS14 = v2e.o subv2e.o v2e_ykfct.o 
OBJECTS14  = $(patsubst %, $(SRC)/%, ${POBJECTS14})
SOURCES14  = ${OBJECTS14:.o=.f} 
LIBS14     = ${LINK_LIB}
PRODUCT14  = Rv2ef
##########################################################################################
POBJECTS15 = mod_utils.o modio.o mod_bs_frb_2e.o mod_h2e.o h2e.o  
OBJECTS15  = $(patsubst %, $(SRC)/%, ${POBJECTS15})
SOURCES15  = ${OBJECTS15:.o=.f90} 
LIBS15     = ${LINK_LIB} ${LINK_LAPACK} ${LINK_BLAS}
PRODUCT15  = Rh2ef
##########################################################################################
POBJECTS16 = mod_utils.o modules_tise.o modio.o mod_bs_frb_2e.o mod_k2e.o k2e.o
OBJECTS16  = $(patsubst %, $(SRC)/%, ${POBJECTS16})
SOURCES16  = ${OBJECTS16:.o=.f90} 
LIBS16     = ${LINK_LIB} ${LINK_LAPACK} ${LINK_BLAS} 
PRODUCT16  = Rk2ef
##########################################################################################
POBJECTS17b = modio.o mod_utils.o mod_d2ebf.o subhdmx2e.o d2ebf.o 
OBJECTS17b  = $(patsubst %, $(SRC)/%, ${POBJECTS17b})
SOURCES17b  = ${OBJECTS17b:.o=.f90} 
LIBS17b     = ${LINK_LIB} ${LINK_BLAS} 
PRODUCT17b  = Rd2ebf
##########################################################################################
POBJECTS17 = modio.o mod_utils.o mod_dmx2ebf.o subhdmx2e.o dmx2ebf.o 
OBJECTS17  = $(patsubst %, $(SRC)/%, ${POBJECTS17})
SOURCES17  = ${OBJECTS17:.o=.f90} 
LIBS17     = ${LINK_LIB} ${LINK_BLAS} 
PRODUCT17  = Rdmx2ebf
##########################################################################################
POBJECTS18 = modio.o mod_dmx2eff.o subhdmx2e.o dmx2eff.o
OBJECTS18  = $(patsubst %, $(SRC)/%, ${POBJECTS18})
SOURCES18  = ${OBJECTS18:.o=.f90} 
LIBS18     = ${LINK_LIB} ${LINK_BLAS} 
PRODUCT18  = Rd2eff
##########################################################################################
POBJECTS19 = modio.o ndmx2ebf.o 
OBJECTS19  = $(patsubst %, $(SRC)/%, ${POBJECTS19})
SOURCES19  = ${OBJECTS19:.o=.f90} 
LIBS19     = ${LINK_LIB} ${LINK_LAPACK} ${LINK_BLAS} 
PRODUCT19  = Rn2ebf
##########################################################################################
POBJECTS20 = modio.o ndmx2eff.o 
OBJECTS20  = $(patsubst %, $(SRC)/%, ${POBJECTS20})
SOURCES20  = ${OBJECTS20:.o=.f90} 
LIBS20     = ${LINK_LIB} ${LINK_LAPACK} ${LINK_BLAS} 
PRODUCT20  = Rn2eff
##########################################################################################


##########################################################################################
# LOPT 
##########################################################################################

##########################################################################################
# single photon cross section (multichannel code)
##########################################################################################


POBJECTS21 = modio.o cs1ph.o 
OBJECTS21  = $(patsubst %, $(SRC)/%, ${POBJECTS21})
SOURCES21  = ${OBJECTS21:.o=.f90} 
LIBS21     = ${LINK_LIB}
PRODUCT21  = Rc2e1

##########################################################################################
# 2-photo cross sections (multichannel code)
##########################################################################################

POBJECTS22 = modio.o cs2ph.o
OBJECTS22  = $(patsubst %, $(SRC)/%, ${POBJECTS22})
SOURCES22  = ${OBJECTS22:.o=.f90} 
LIBS22     = ${LINK_LIB} ${LINK_LAPACK} ${LINK_BLAS} 
PRODUCT22  = Rc2e2

##########################################################################################
# single photon cross section (fxd code)
##########################################################################################

POBJECTS23 = modio.o cs2e1ph_b.o 
OBJECTS23  = $(patsubst %, $(SRC)/%, ${POBJECTS23})
SOURCES23  = ${OBJECTS23:.o=.f} 
LIBS23     = ${LINK_LIB}
PRODUCT23  = Rcs2e1ph_b

###########################################################################################
# 2-photo cross sections (fxd code)
##########################################################################################

POBJECTS24 = modio.o cs2e2ph_b.o
OBJECTS24  = $(patsubst %, $(SRC)/%, ${POBJECTS24})
SOURCES24  = ${OBJECTS24:.o=.f} 
LIBS24     = ${LINK_LIB} 
PRODUCT24  = Rcs2e2ph_b

##########################################################################################
# crmr
##########################################################################################
SOURCES25	= icsNph.f #subio_lopt.f 
OBJECTS25 	= $(patsubst %, $(SRC)/%, ${SOURCES25:.f=.o})
PRODUCT25	= RcsNph_i
LIBS25   	= ${LINK_LIB}

##########################################################################################
SOURCES26	= csNph.f subcsNph.f 
OBJECTS26 	= $(patsubst %, $(SRC)/%, ${SOURCES26:.f=.o}) 
PRODUCT26	= RcsNph_l   
LIBS26   	= ${LINK_BLAS}

##########################################################################################
SOURCES27	= csNph_merge_r.f90 
OBJECTS27 	= $(patsubst %, $(SRC)/%, ${SOURCES27:.f90=.o}) 
PRODUCT27	= RcsNph_merge_r   
LIBS27  	= ${LINK_SLATEC} 

##########################################################################################
SOURCES28	= mod_precision.f90 mod_units.f90 mod_ang_lopt.f90 mod_utils_lopt.f90 csNph_total.f90  
OBJECTS28 	= $(patsubst %, $(SRC)/%, ${SOURCES28:.f90=.o}) 
PRODUCT28	= RcsNph_t
LIBS28   	= 

##########################################################################################
SOURCES29a	= stark.f 
OBJECTS29a 	= $(patsubst %, $(SRC)/%, ${SOURCES29a:.f=.o})
PRODUCT29a	= Rstark
LIBS29a   	= 

##########################################################################################
SOURCES29b	= shift.f
OBJECTS29b 	= $(patsubst %, $(SRC)/%, ${SOURCES29b:.f=.o}) 
PRODUCT29b	= Rshift
LIBS29b   	= 
##########################################################################################



##########################################################################################
#                            tdse-2e
##########################################################################################

POBJECTS30 = mod_dynamic_array.o mod_field.o mod_utils.o \
              mod_tdse_bs.o subtdse_bs.o tdse_bs_temp.o 
OBJECTS30  = $(patsubst %, $(SRC)/%, ${POBJECTS30})
SOURCES30  = ${OBJECTS30:.o=.f90} 
LIBS30     = ${LINK_LAPACK} ${LINK_BLAS} ${LINK_NAG17} ${LINK_LIB} ${LINK_NETCDF_F} 
PROFFLAG30 = -Wmaybe-uninitialized 
#${LINK_ODE}
PRODUCT30  = Rtdse_bs

##########################################################################################
POBJECTS30a = mod_tdse_bs.o tdse_bs_pes.o 
OBJECTS30a  = $(patsubst %, $(SRC)/%, ${POBJECTS30a})
SOURCES30a  = ${OBJECTS30a:.o=.f90} 
LIBS30a     = ${LINK_LIB} ${LINK_NETCDF_F} 
PRODUCT30a  = Rtdse_bs_pes
##########################################################################################
POBJECTS30b = mod_tdse_bs.o tdse_bs_pes.o #tdse_bs_hhg.o (change to hhg file when you find this file)
OBJECTS30b  = $(patsubst %, $(SRC)/%, ${POBJECTS30b})
SOURCES30b  = ${OBJECTS30b:.o=.f90} 
LIBS30b     = ${LINK_LIB} ${LINK_NAG17} ${LINK_NETCDF_F}
PRODUCT30b  = Rtdse_hhg
##########################################################################################
POBJECTS30c = modio.o\
             bs1e_parameter.o mod_utils-v1.o mod_bs_frb_2e.o \
             mod_tdse_bs.o mod_tdse_bs_pop2.o tdse_bs_pop_timetrace.o
OBJECTS30c  = $(patsubst %, $(SRC)/%, ${POBJECTS30c})
SOURCES30c  = ${OBJECTS30c:.o=.f90} 
LIBS30c     = ${LINK_LIB} ${LINK_NAG17} ${LINK_NETCDF_F}
PRODUCT30c  = Rtdse_bs_pop_timetrace
##########################################################################################
POBJECTS30d = modio.o\
             bs1e_parameter.o mod_utils-v1.o mod_bs_frb_2e.o \
             mod_tdse_bs.o tdse_bs_pop_v0.o
OBJECTS30d  = $(patsubst %, $(SRC)/%, ${POBJECTS30d})
SOURCES30d  = ${OBJECTS30d:.o=.f90} 
LIBS30d     = ${LINK_LIB} ${LINK_NAG17} ${LINK_NETCDF_F}
PRODUCT30d  = Rtdse_bs_pop_v0
##########################################################################################
POBJECTS31 = subtdse_bs_frb_2e_cos_s.o tdse_bs_frb_2e_cos_s.o
OBJECTS31  = $(patsubst %, $(SRC)/%, ${POBJECTS31})
SOURCES31  = ${OBJECTS31:.o=.f90} 
LIBS31     = ${LINK_LIB} ${LINK_LAPACK} ${LINK_BLAS} ${LINK_NAG17} 
PRODUCT31  = Rtdse_bs_frb_2e_cos_s
# (double-cos)
##########################################################################################
POBJECTS32 = subtdse_bs_frb_2e.o tdse_bs_frb_2e.o 
OBJECTS32  = $(patsubst %, $(SRC)/%, ${POBJECTS32})
SOURCES32  = ${OBJECTS32:.o=.f90} 
LIBS32     = ${LINK_LIB} ${LINK_LAPACK} ${LINK_BLAS} ${LINK_NAG17} 
PRODUCT32  = Rtdse_bs_frb_2e #Rtdse_bs_frb_2e
##########################################################################################
POBJECTS32a = subtdse_bs_frb_2e_ramp_d.o tdse_bs_frb_2e_ramp_d.o 
OBJECTS32a  = $(patsubst %, $(SRC)/%, ${POBJECTS32a})
SOURCES32a  = ${OBJECTS32a:.o=.f90} 
LIBS32a     = ${LINK_LIB} ${LINK_LAPACK} ${LINK_BLAS} ${LINK_NAG17} 
PRODUCT32a  = Rtdse_bs_frb_ramp_d
##########################################################################################
POBJECTS33 = subtdse_bs_frb_2e_pad.o tdse_bs_frb_2e_pad.o 
OBJECTS33  = $(patsubst %, $(SRC)/%, ${POBJECTS33})
SOURCES33  = ${OBJECTS33:.o=.f90} 
LIBS33     = ${LINK_LIB} 
PRODUCT33  = Rpad
##########################################################################################
POBJECTS34 = tdse_bs_frb_2e_read_dmx.o
OBJECTS34  = $(patsubst %, $(SRC)/%, ${POBJECTS34})
SOURCES34  = ${OBJECTS34:.o=.f90} 
LIBS34     = ${LINK_LIB} 
PRODUCT34  = Rtdse_bs_frb_2e_read_dmx
##########################################################################################
POBJECTS35 = tdse_bs_fxd_2e_read_coe.o #tdse_bs_frb_2e_read_coe.o
OBJECTS35  = $(patsubst %, $(SRC)/%, ${POBJECTS35})
SOURCES35  = ${OBJECTS35:.o=.f90} 
LIBS35     = ${LINK_LIB} 
PRODUCT35  = Rtdse_bs_fxd_read_coe
##########################################################################################
POBJECTS36 = pcolor.o
OBJECTS36  = $(patsubst %, $(SRC)/%, ${POBJECTS36})
SOURCES36  = ${OBJECTS36:.o=.f90} 
LIBS36     = ${LINK_LIB}
PRODUCT36  = Rpcolor
##########################################################################################
POBJECTS37 = mod_precision.o bs1e_parameter.o mod_utils-v1.o mod_bs_frb_2e.o\
            mod_tdse_bs.o mod_tdse_bs_pop2.o rd2e_double_ionisation_total.o
OBJECTS37  = $(patsubst %, $(SRC)/%, ${POBJECTS37})           
SOURCES37  = ${OBJECTS37:.o=.f90} 
LIBS37     = ${LINK_LIB} ${LINK_LAPACK} ${LINK_NAG17} ${LINK_SLATEC} ${LINK_NETCDF_F} -fopenmp
PRODUCT37  = rd2e_double_ionisation_total
# (parallel) (spse)
##########################################################################################
POBJECTS37a = mod_precision.o bs1e_parameter.o mod_utils-v1.o mod_bs_frb_2e.o \
              mod_tdse_bs.o mod_tdse_bs_pop2.o mod_field.o rd2e_partial_wave_double_ionisation.o
OBJECTS37a  = $(patsubst %, $(SRC)/%, ${POBJECTS37a})
SOURCES37a  = ${OBJECTS37a:.o=.f90} 
LIBS37a     = ${LINK_LIB} ${LINK_LAPACK} ${LINK_NAG17} ${LINK_SLATEC} ${LINK_NETCDF_F} -fopenmp
PRODUCT37a  = rd2e_partial_wave_double_ionisation_t
##########################################################################################
POBJECTS37b = mod_precision.o bs1e_parameter.o mod_spherical_harmonics.o anglib.o mod_utils-v1.o mod_bs_frb_2e.o \
              mod_tdse_bs.o mod_tdse_bs_pop2.o mod_core_phase_shift.o subtdse_bs_frb_2e_pad.o pad2e.o
OBJECTS37b  = $(patsubst %, $(SRC)/%, ${POBJECTS37b})
SOURCES37b  = ${OBJECTS37b:.o=.f90} 
LIBS37b     = ${LINK_LIB} ${LINK_LAPACK} ${LINK_NAG17} ${LINK_SLATEC} ${LINK_NETCDF_F} -fopenmp
PRODUCT37b  = pad2e
##########################################################################################
POBJECTS37c = mod_w2e.o mod_precision.o bs1e_parameter.o mod_utils-v1.o mod_bs_frb_2e.o \
              mod_tdse_bs.o mod_tdse_bs_pop2.o v12t.o
OBJECTS37c  = $(patsubst %, $(SRC)/%, ${POBJECTS37c})
SOURCES37c  = ${OBJECTS37c:.o=.f90} 
LIBS37c     = ${LINK_LIB} ${LINK_LAPACK} ${LINK_NAG17} ${LINK_SLATEC} ${LINK_NETCDF_F} -fopenmp
PRODUCT37c  = Rv12t
##########################################################################################
POBJECTS37d = mod_w2e.o mod_precision.o bs1e_parameter.o mod_utils-v1.o mod_bs_frb_2e.o \
              mod_tdse_bs.o mod_tdse_bs_pop2.o enlnl.o
OBJECTS37d  = $(patsubst %, $(SRC)/%, ${POBJECTS37d})
SOURCES37d  = ${OBJECTS37d:.o=.f90} 
LIBS37d     = ${LINK_LIB} ${LINK_LAPACK} ${LINK_NAG17} ${LINK_SLATEC} ${LINK_NETCDF_F} -fopenmp
PRODUCT37d  = Renlnl
##########################################################################################
# write a test bessel function to a file
##########################################################################################
POBJECTS38 = mod_precision.o bs1e_parameter.o mod_spherical_harmonics.o anglib.o mod_utils-v1.o mod_bs_frb_2e.o \
             mod_tdse_bs.o mod_tdse_bs_pop.o bessel_print.o
OBJECTS38  = $(patsubst %, $(SRC)/%, ${POBJECTS38})
SOURCES38  = ${OBJECTS38:.o=.f90} 
LIBS38     = ${LINK_LIB} ${LINK_LAPACK} ${LINK_NAG17} ${LINK_SLATEC} ${LINK_NETCDF_F} -fopenmp
PRODUCT38  = bessel_print
##########################################################################################
# Seperate radial wf file for given l into individual files
##########################################################################################
POBJECTS38a = mod_precision.o bs1e_parameter.o mod_spherical_harmonics.o anglib.o mod_utils-v1.o mod_bs_frb_2e.o \
              mod_tdse_bs.o mod_tdse_bs_pop.o radial_function_separator.o
OBJECTS38a  = $(patsubst %, $(SRC)/%, ${POBJECTS38a})
SOURCES38a  = ${OBJECTS38a:.o=.f90} 
LIBS38a     = ${LINK_LIB} ${LINK_LAPACK} ${LINK_NAG17} ${LINK_SLATEC} ${LINK_NETCDF_F} -fopenmp
PRODUCT38a  = radial_separator
##########################################################################################
POBJECTS37e = mod_w2e.o mod_precision.o bs1e_parameter.o mod_utils-v1.o mod_bs_frb_2e.o \
              mod_tdse_bs.o mod_tdse_bs_pop2.o v12t_method9HT.o
OBJECTS37e  = $(patsubst %, $(SRC)/%, ${POBJECTS37e})
SOURCES37e  = ${OBJECTS37e:.o=.f90} 
LIBS37e     = ${LINK_LIB} ${LINK_LAPACK} ${LINK_NAG17} ${LINK_SLATEC} ${LINK_NETCDF_F} -fopenmp
PRODUCT37e  = v12t_method9HT
##########################################################################################


##########################################################################################
# Define batches of executables that are created via make commands, e.g make fxd_exe
# will compile all the executables in the fxd_exe batch for fixed boundary conditions
##########################################################################################

PROD = 1 2 3 4 5 \
       6 7 8 9 10 10a \
       14 15 16 17 17b \
       18 19 20 21 22 \
       23 24 25 26 27 \
       28 29a 29b 30 30a \
       31 32 32a 33 33a \
       34 35 36

PRODUCT = ${PRODUCT1} ${PRODUCT2} ${PRODUCT3} ${PRODUCT4} ${PRODUCT4a} \
          ${PRODUCT5} ${PRODUCT6} ${PRODUCT7} ${PRODUCT8} ${PRODUCT9} \
          ${PRODUCT10} ${PRODUCT10a} ${PRODUCT14} ${PRODUCT15} ${PRODUCT16} \
          ${PRODUCT17} ${PRODUCT17b} ${PRODUCT18} ${PRODUCT19} ${PRODUCT20} \
          ${PRODUCT21} ${PRODUCT22} ${PRODUCT23} ${PRODUCT24} ${PRODUCT25} \
          ${PRODUCT26} ${PRODUCT27} ${PRODUCT28} ${PRODUCT29a} ${PRODUCT29b}

# change from 3a back to 3 at some point (EDITED BY ADAM TO INCLUDE WRITING OF ALL 1e WAVFUNCTIONS ON 24/01/19)
FXD_EXE = ${PRODUCT1} ${PRODUCT2} ${PRODUCT3a} ${PRODUCT4} ${PRODUCT4b} \
          ${PRODUCT5} ${PRODUCT6} ${PRODUCT7} ${PRODUCT8} ${PRODUCT9} \
          ${PRODUCT10} ${PRODUCT10a}

FREE_EXE = ${PRODUCT14} ${PRODUCT15} ${PRODUCT16} ${PRODUCT17} ${PRODUCT17b} \
           ${PRODUCT18} ${PRODUCT19} ${PRODUCT20}

LOPT_EXE = ${PRODUCT21} ${PRODUCT22} ${PRODUCT23} ${PRODUCT24} ${PRODUCT25} \
           ${PRODUCT26} ${PRODUCT27} ${PRODUCT28} ${PRODUCT29} ${PRODUCT29a} \
           ${PRODUCT29b}

TDSE_EXE = ${PRODUCT30} ${PRODUCT30a} ${PRODUCT30b} ${PRODUCT30c} ${PRODUCT30d} \
           ${PRODUCT31} ${PRODUCT32} ${PRODUCT32a} ${PRODUCT33} ${PRODUCT34} \
           ${PRODUCT35} ${PRODUCT36}


free_exe: ${FREE_EXE}
fxd_exe:  ${FXD_EXE}
lopt_exe: ${LOPT_EXE}
tdse_exe: ${TDSE_EXE}
all:  ${LPRODUCT} ${PRODUCT} 



${MPRODUCT}: ${MOBJECTS}
	cd ${MOD}
	$(F90) $(F90FLAGS) $(MOBJECTS) 
#	ar -cru ${LIBNAME} ${MOBJECTS}
#	mkdir -p ${LIB}
#	cp  ${LIBNAME} ${LIB}
#	mv  ${LIBNAME} ${AMO_LIB}

${LPRODUCT}: ${LOBJECTS}
	ar -cru ${LIBNAME} ${LOBJECTS}
	mkdir -p ${LIB}
	mv  ${LIBNAME} ${LIB}
#	mv  ${LIBNAME} ${AMO_LIB}

#${PRODUCT0}: $(OBJECTS0) 
#	 $(CXX) $(PROFFLAG) $(CXXFLAGS) -o $(PRODUCT0) $(OBJECTS0) $(LIBS0)
#	 cp ${PRODUCT0} ${BIN}
#	 mv ${PRODUCT0} ${AMO_BIN}

${PRODUCT1}: $(OBJECTS1) 
	 $(F90) $(PROFFLAG1) $(F90FLAGS) -o $(PRODUCT1) $(OBJECTS1) $(LIBS1) ;\
	 mv ${PRODUCT1} ${BIN}

${PRODUCT2}: $(OBJECTS2) 
	 $(F90) $(PROFFLAG2) $(F90FLAGS) -o $(PRODUCT2) $(OBJECTS2) $(LIBS2)
	 mv ${PRODUCT2} ${BIN}
#	 mv ${PRODUCT2} ${AMO_BIN}

${PRODUCT2b}: $(OBJECTS2b) 
	 $(F90) $(PROFFLAG2b) $(F90FLAGS) -o $(PRODUCT2b) $(OBJECTS2b) $(LIBS2b)
	 mv ${PRODUCT2b} ${BIN}
#	 mv ${PRODUCT2} ${AMO_BIN}

${PRODUCT3}: $(OBJECTS3) 
	 $(F90) $(PROFFLAG3) $(F90FLAGS) -o $(PRODUCT3) $(OBJECTS3) $(LIBS3)
	 mv ${PRODUCT3} ${BIN}
#	 mv ${PRODUCT3} ${AMO_BIN}

${PRODUCT3a}: $(OBJECTS3a) 
	 $(F90) $(PROFFLAG3a) $(F90FLAGS) -o $(PRODUCT3a) $(OBJECTS3a) $(LIBS3a)
	 mv ${PRODUCT3a} ${BIN}
#	 mv ${PRODUCT3a} ${AMO_BIN}

${PRODUCT4}: $(OBJECTS4) 
	 $(F90) $(PROFFLAG4) $(F90FLAGS) -o $(PRODUCT4) $(OBJECTS4) $(LIBS4)
	 mv ${PRODUCT4} ${BIN}
#	 mv ${PRODUCT4} ${AMO_BIN}

${PRODUCT4b}: $(OBJECTS4b) 
	 $(F90) $(PROFFLAG4b) $(F90FLAGS) -o $(PRODUCT4b) $(OBJECTS4b) $(LIBS4b)
	 mv ${PRODUCT4b} ${BIN}
#	 mv ${PRODUCT4b} ${AMO_BIN}
# f90 version of 4

${PRODUCT4a}: $(OBJECTS4a) 
	 $(F90) $(PROFFLAG4a) $(F90FLAGS) -o $(PRODUCT4a) $(OBJECTS4a) $(LIBS4a)
	 mv ${PRODUCT4a} ${BIN}
#	 mv ${PRODUCT4a} ${AMO_BIN}

${PRODUCT5}: $(OBJECTS5) 
	 $(F90) $(PROFFLAG5) $(F90FLAGS) -o $(PRODUCT5) $(OBJECTS5) $(LIBS5)
	 mv ${PRODUCT5} ${BIN}
#	 mv ${PRODUCT5} ${AMO_BIN}

${PRODUCT6}: $(OBJECTS6) 
	 $(F90) $(PROFFLAG6) $(F90FLAGS) -o $(PRODUCT6) $(OBJECTS6) $(LIBS6)
	 mv ${PRODUCT6} ${BIN}
#	 mv ${PRODUCT6} ${AMO_BIN}

${PRODUCT7}: $(OBJECTS7) 
	 $(F90) $(PROFFLAG7) $(F90FLAGS) -o $(PRODUCT7) $(OBJECTS7) $(LIBS7)
	 mv ${PRODUCT7} ${BIN}
#	 mv ${PRODUCT7} ${AMO_BIN}

${PRODUCT8}: $(OBJECTS8) 
	 $(F90) $(PROFFLAG8) $(F90FLAGS) -o $(PRODUCT8) $(OBJECTS8) $(LIBS8)
	 mv ${PRODUCT8} ${BIN}
#	 mv ${PRODUCT8} ${AMO_BIN}

${PRODUCT9}: $(OBJECTS9) 
	 $(F90) $(PROFFLAG9) $(F90FLAGS) -o $(PRODUCT9) $(OBJECTS9) $(LIBS9)
	 mv ${PRODUCT9} ${BIN}
#	 mv ${PRODUCT9} ${AMO_BIN}

${PRODUCT10}: $(OBJECTS10) 
	 $(F90) $(PROFFLAG10) $(F90FLAGS) -o $(PRODUCT10) $(OBJECTS10) $(LIBS10)
	 mv ${PRODUCT10} ${BIN}
#	 mv ${PRODUCT10} ${AMO_BIN}

${PRODUCT10a}: $(OBJECTS10a) 
	 $(F90) $(PROFFLAG10a) $(F90FLAGS) -o $(PRODUCT10a) $(OBJECTS10a) $(LIBS10a)
	 mv ${PRODUCT10a} ${BIN}
#	 mv ${PRODUCT10a} ${AMO_BIN}




${PRODUCT14}: $(OBJECTS14) 
	 $(F90) $(PROFFLAG14) $(F90FLAGS) -o $(PRODUCT14) $(OBJECTS14) $(LIBS14)
	 mv ${PRODUCT14} ${BIN}
#	 mv ${PRODUCT14} ${AMO_BIN}

${PRODUCT15}: $(OBJECTS15) 
	 $(F90) $(PROFFLAG15) $(F90FLAGS) -o $(PRODUCT15) $(OBJECTS15) $(LIBS15)
	 mv ${PRODUCT15} ${BIN}
#	 mv ${PRODUCT15} ${AMO_BIN}

${PRODUCT16}: $(OBJECTS16) 
	 $(F90) $(PROFFLAG16) $(F90FLAGS) -o $(PRODUCT16) $(OBJECTS16) $(LIBS16)
	 mv ${PRODUCT16} ${BIN}
#	 mv ${PRODUCT16} ${AMO_BIN}

${PRODUCT17}: $(OBJECTS17) 
	 $(F90) $(PROFFLAG17) $(F90FLAGS) -o $(PRODUCT17) $(OBJECTS17) $(LIBS17)
	 mv ${PRODUCT17} ${BIN}
#	 mv ${PRODUCT17} ${AMO_BIN}

${PRODUCT17b}: $(OBJECTS17b) 
	 $(F90) $(PROFFLAG17b) $(F90FLAGS) -o $(PRODUCT17b) $(OBJECTS17b) $(LIBS17b)
	 mv ${PRODUCT17b} ${BIN}
#	 mv ${PRODUCT17b} ${AMO_BIN}

${PRODUCT18}: $(OBJECTS18) 
	 $(F90) $(PROFFLAG18) $(F90FLAGS) -o $(PRODUCT18) $(OBJECTS18) $(LIBS18)
	 mv ${PRODUCT18} ${BIN}
#	 mv ${PRODUCT18} ${AMO_BIN}

${PRODUCT19}: $(OBJECTS19) 
	 $(F90) $(PROFFLAG19) $(F90FLAGS) -o $(PRODUCT19) $(OBJECTS19) $(LIBS19)
	 mv ${PRODUCT19} ${BIN}
#	 mv ${PRODUCT19} ${AMO_BIN}

${PRODUCT20}: $(OBJECTS20) 
	 $(F90) $(PROFFLAG20) $(F90FLAGS) -o $(PRODUCT20) $(OBJECTS20) $(LIBS20)
	 mv ${PRODUCT20} ${BIN}
#	 mv ${PRODUCT20} ${AMO_BIN}


##########################################################################################
# LOPT        
##########################################################################################

${PRODUCT21}: $(OBJECTS21) 
	 $(F90) $(PROFFLAG21) $(F90FLAGS) -o $(PRODUCT21) $(OBJECTS21) $(LIBS21)
	 mv ${PRODUCT21} ${BIN}
#	 mv ${PRODUCT21} ${AMO_BIN}

${PRODUCT22}: $(OBJECTS22) 
	 $(F90) $(PROFFLAG22) $(F90FLAGS) -o $(PRODUCT22) $(OBJECTS22) $(LIBS22)
	 mv ${PRODUCT22} ${BIN}
#	 mv ${PRODUCT22} ${AMO_BIN}

${PRODUCT23}: $(OBJECTS23) 
	 $(F90) $(PROFFLAG23) $(F90FLAGS) -o $(PRODUCT23) $(OBJECTS23) $(LIBS23)
	 mv ${PRODUCT23} ${BIN}
#	 mv ${PRODUCT23} ${AMO_BIN}

${PRODUCT24}: $(OBJECTS24) 
	 $(F90) $(PROFFLAG24) $(F90FLAGS) -o $(PRODUCT24) $(OBJECTS24) $(LIBS24)
	 mv ${PRODUCT24} ${BIN}
#	 mv ${PRODUCT24} ${AMO_BIN}

${PRODUCT25}: $(OBJECTS25) 
	 $(F90) $(PROFFLAG25) $(F90FLAGS) -o $(PRODUCT25) $(OBJECTS25) $(LIBS25)
	 mv ${PRODUCT25} ${BIN}
#	 mv ${PRODUCT25} ${AMO_BIN}

${PRODUCT26}: $(OBJECTS26) 
	 $(F90) $(PROFFLAG26) $(F90FLAGS) -o $(PRODUCT26) $(OBJECTS26) $(LIBS26)
	 mv ${PRODUCT26} ${BIN}
#	 mv ${PRODUCT26} ${AMO_BIN}

${PRODUCT27}: $(OBJECTS27) 
	 $(F90) $(PROFFLAG27) $(F90FLAGS) -o $(PRODUCT27) $(OBJECTS27) $(LIBS27)
	 mv ${PRODUCT27} ${BIN}
#	 mv ${PRODUCT27} ${AMO_BIN}

${PRODUCT28}: $(OBJECTS28) 
	 $(F90) $(PROFFLAG28) $(F90FLAGS) -o $(PRODUCT28) $(OBJECTS28) $(LIBS28)
	 mv ${PRODUCT28} ${BIN}
#	 mv ${PRODUCT28} ${AMO_BIN}

${PRODUCT29a}: $(OBJECTS29a) 
	 $(F90) $(PROFFLAG29a) $(F90FLAGS) -o $(PRODUCT29a) $(OBJECTS29a) $(LIBS29a)
	 mv ${PRODUCT29a} ${BIN}
#	 mv ${PRODUCT29} ${AMO_BIN}

${PRODUCT29b}: $(OBJECTS29b) 
	 $(F90) $(PROFFLAG29b) $(F90FLAGS) -o $(PRODUCT29b) $(OBJECTS29b) $(LIBS29b)
	 mv ${PRODUCT29b} ${BIN}
#	 mv ${PRODUCT29b} ${AMO_BIN}


##########################################################################################
# tdse
##########################################################################################

${PRODUCT30}: $(OBJECTS30) 
	 $(F90) $(PROFFLAG30) $(F90FLAGS) -o $(PRODUCT30) $(OBJECTS30) $(LIBS30)
	 mv ${PRODUCT30} ${BIN}
#	 mv ${PRODUCT30} ${AMO_BIN}

# pes
${PRODUCT30a}: $(OBJECTS30a) 
	 $(F90) $(PROFFLAG30a) $(F90FLAGS) -o $(PRODUCT30a) $(OBJECTS30a) $(LIBS30a)
	 mv ${PRODUCT30a} ${BIN}
#	 mv ${PRODUCT30a} ${AMO_BIN}

# hhg
${PRODUCT30b}: $(OBJECTS30b) 
	 $(F90) $(PROFFLAG30b) $(F90FLAGS) -o $(PRODUCT30b) $(OBJECTS30b) $(LIBS30b)
	 mv ${PRODUCT30b} ${BIN}
#	 mv ${PRODUCT30b} ${AMO_BIN}

${PRODUCT30c}: $(OBJECTS30c) 
	 $(F90) $(PROFFLAG30c) $(F90FLAGS) -o $(PRODUCT30c) $(OBJECTS30c) $(LIBS30c)
	 mv ${PRODUCT30c} ${BIN}
#	 mv ${PRODUCT30c} ${AMO_BIN}

${PRODUCT30d}: $(OBJECTS30d) 
	 $(F90) $(PROFFLAG30d) $(F90FLAGS) -o $(PRODUCT30d) $(OBJECTS30d) $(LIBS30d)
	 mv ${PRODUCT30d} ${BIN}
#	 mv ${PRODUCT30d} ${AMO_BIN}

${PRODUCT31}: $(OBJECTS31) 
	 $(F90) $(PROFFLAG31) $(F90FLAGS) -o $(PRODUCT31) $(OBJECTS31) $(LIBS31) ;\
	 mv ${PRODUCT31} ${BIN} ;\

${PRODUCT32}: $(OBJECTS32) 
	 $(F90) $(PROFFLAG32) $(F90FLAGS) -o $(PRODUCT32) $(OBJECTS32) $(LIBS32)
	 mv ${PRODUCT32} ${BIN}
#	 mv ${PRODUCT32} ${AMO_BIN}

${PRODUCT32b}: $(OBJECTS32b) 
	 $(F90) $(PROFFLAG32b) $(F90FLAGS) -o $(PRODUCT32b) $(OBJECTS32b) $(LIBS32b)
	 mv ${PRODUCT32b} ${BIN}
#	 mv ${PRODUCT2b} ${AMO_BIN}

${PRODUCT32a}: $(OBJECTS32a) 
	 $(F90) $(PROFFLAG32a) $(F90FLAGS) -o $(PRODUCT32a) $(OBJECTS32a) $(LIBS32a)
	 mv ${PRODUCT32a} ${BIN}
#	 mv ${PRODUCT32a} ${AMO_BIN}

${PRODUCT33}: $(OBJECTS33) 
	$(F90) $(PROFFLAG33) $(F90FLAGS) -o $(PRODUCT33) $(OBJECTS33) $(LIBS33)
	 mv ${PRODUCT33} ${BIN}
#	 mv ${PRODUCT33} ${AMO_BIN}

${PRODUCT34}: $(OBJECTS34) 
	$(F90) $(PROFFLAG34) $(F90FLAGS) -o $(PRODUCT34) $(OBJECTS34) $(LIBS34)
	 mv ${PRODUCT34} ${BIN}
#	 mv ${PRODUCT34} ${AMO_BIN}

${PRODUCT35}: $(OBJECTS35) 
	 $(F90) $(PROFFLAG35) $(F90FLAGS) -o $(PRODUCT35) $(OBJECTS35) $(LIBS35)
	 mv ${PRODUCT35} ${BIN}
#	 mv ${PRODUCT35} ${AMO_BIN}

${PRODUCT36}: $(OBJECTS36) 
	 $(F90) $(PROFFLAG36) $(F90FLAGS) -o $(PRODUCT36) $(OBJECTS36) $(LIBS36)
	 mv ${PRODUCT36} ${BIN}
#	 mv ${PRODUCT36} ${AMO_BIN}

${PRODUCT37}: $(OBJECTS37) 
	 $(F90) $(PROFFLAG37) $(F90FLAGS) -o $(PRODUCT37) $(OBJECTS37) $(LIBS37)
	 mv ${PRODUCT37} ${BIN}
#	 mv ${PRODUCT37} ${AMO_BIN}
${PRODUCT37a}: $(OBJECTS37a) 
	 $(F90) $(PROFFLAG37a) $(F90FLAGS) -o $(PRODUCT37a) $(OBJECTS37a) $(LIBS37a)
	 mv ${PRODUCT37a} ${BIN}
#	 mv ${PRODUCT37a} ${AMO_BIN}

${PRODUCT37b}: $(OBJECTS37b) 
	 $(F90) $(PROFFLAG37b) $(F90FLAGS) -o $(PRODUCT37b) $(OBJECTS37b) $(LIBS37b)
	 mv ${PRODUCT37b} ${BIN}
#	 mv ${PRODUCT37b} ${AMO_BIN}

${PRODUCT37c}: $(OBJECTS37c) 
	 $(F90) $(PROFFLAG37c) $(F90FLAGS) -o $(PRODUCT37c) $(OBJECTS37c) $(LIBS37c)
	 mv ${PRODUCT37c} ${BIN}
#	 mv ${PRODUCT37c} ${AMO_BIN}

${PRODUCT37d}: $(OBJECTS37d) 
	 $(F90) $(PROFFLAG37d) $(F90FLAGS) -o $(PRODUCT37d) $(OBJECTS37d) $(LIBS37d)
	 mv ${PRODUCT37d} ${BIN}
#	 mv ${PRODUCT37d} ${AMO_BIN}

${PRODUCT37e}: $(OBJECTS37e) 
	 $(F90) $(PROFFLAG37e) $(F90FLAGS) -o $(PRODUCT37e) $(OBJECTS37e) $(LIBS37e)
	 mv ${PRODUCT37e} ${BIN}
#	 mv ${PRODUCT37e} ${AMO_BIN}

${PRODUCT38}: $(OBJECTS38) 
	 $(F90) $(PROFFLAG38) $(F90FLAGS) -o $(PRODUCT38) $(OBJECTS38) $(LIBS38)
	 mv ${PRODUCT38} ${BIN}
#	 mv ${PRODUCT38} ${AMO_BIN}
${PRODUCT38a}: $(OBJECTS38a) 
	 $(F90) $(PROFFLAG38a) $(F90FLAGS) -o $(PRODUCT38a) $(OBJECTS38a) $(LIBS38a)
	 mv ${PRODUCT38a} ${BIN}
#	 mv ${PRODUCT38a} ${AMO_BIN}


# pattern rules
c      = $(C)   $(CFLAGS)   $(INCLUDE)  -c $<  -o $@
cc     = $(CXX) $(CXXFLAGS) $(INCLUDE)  -c $<  -o $@
f90    = $(F90) $(F90FLAGS) $(INCLUDE)  -c $<  -o $@
f77    = $(FC)  $(FCFLAGS)  $(INCLUDE)  -c $<  -o $@
mpif90 = $(MPI) $(F90FLAGS) $(INCLUDE)  -c $<  -o $@

%.o:%.c
	${c}
%.o:%.C
	${cc}
%.o:%.f
	${f90} -J${SRC}
%.o:%.f90
	${f90} -J${SRC}
%.mpo:%.f90
	${mpif90}


list:
	-@echo "#" 
	-@echo "#" 
	-@echo "#          h1e (fxd/free)"
	-@echo "#" 
	-@echo "#" 
	-@echo "#  1 :  " ${PRODUCT1}    " o:" ${OBJECTS1}
	-@echo "#  2 :  h1e  eigen energies/ bsp-coefficients : " ${PRODUCT2}    " o:" ${OBJECTS2}
	-@echo "#  3 :  w1e/d1e 1e-wavefunctions/dipoles      : " ${PRODUCT3}    " o:" ${OBJECTS3}
	-@echo "#" 
	-@echo "#" 
	-@echo "#          h2e (fxd)" 
	-@echo "#" 
	-@echo "#" 
	-@echo "#  4 :  ci (1/r_12) matrix elements for h2eb  : " ${PRODUCT4}    " o:" ${OBJECTS4}
	-@echo "#  5 :  h2e eigen energies/coefficients       : " ${PRODUCT5}    " o:" ${OBJECTS5}
	-@echo "#  6 :  w2e 2e-wavefunctions                  : " ${PRODUCT6}    " o:" ${OBJECTS6}
	-@echo "#  6 :  w2e 2e-wavefunctions                  : " ${PRODUCT7}    " o:" ${OBJECTS7}
	-@echo "#  8 :  normalization (hnorm)	                " ${PRODUCT8}    " o:" ${OBJECTS8}
	-@echo "#  9 :  dipoles normalization                 : " ${PRODUCT9}    " o:" ${OBJECTS9}
	-@echo "# 10 :  netCDF dipole files                   : " ${PRODUCT10}   " o:" ${OBJECTS10}
	-@echo "# 10a : netCDF test                           : " ${PRODUCT10a}  " o:" ${OBJECTS10a}	
	-@echo "#" 
	-@echo "#" 
	-@echo "#          h2e (free)" 
	-@echo "#" 
	-@echo "#" 
	-@echo "# 14 : ci (1/r_12) matrix elements for h2ef   : " ${PRODUCT14}   " o:" ${OBJECTS14}
	-@echo "# 15 : h2e eigen energies/coefficients        : " ${PRODUCT15}   " o:" ${OBJECTS15}
	-@echo "# 16 : k2e-matrix   normalization             : " ${PRODUCT16}   " o:" ${OBJECTS16}
	-@echo "# 17 : bound- free 2e dipoles                 : " ${PRODUCT17}   " o:" ${OBJECTS17}
	-@echo "# 17b: bound- free 2e dipoles                 : " ${PRODUCT17b}   " o:" ${OBJECTS17b}
	-@echo "# 18 : free - free 2e dipoles                 : " ${PRODUCT18}    " o:" ${OBJECTS18}
	-@echo "# 19 : normalization of b-f 2e dipoles        : " ${PRODUCT19}    " o:" ${OBJECTS19}
	-@echo "# 20 : normalization of f-f 2e dipoles        : " ${PRODUCT20}    " o:" ${OBJECTS20}
	-@echo "#" 
	-@echo "#" 
	-@echo "#             LOPT" 
	-@echo "#" 
	-@echo "#" 
	-@echo "# 21  : single-photon cross section free b.c.  : " ${PRODUCT21}    " o:" ${OBJECTS21}
	-@echo "# 22  : two-photon cross section    free b.b   : " ${PRODUCT22}    " o:" ${OBJECTS22}
	-@echo "# 23  : single-photon cross section fxd  b.c.  : " ${PRODUCT23}    " o:" ${OBJECTS23}
	-@echo "# 24  : two-photon cross section    fxd  b.c   : " ${PRODUCT24}    " o:" ${OBJECTS24}
	-@echo "# 25  : csN-photon interface                   : " ${PRODUCT25}    " o:" ${OBJECTS25}
	-@echo "# 26  : csN-photon (main program)              : " ${PRODUCT26}    " o:" ${OBJECTS26}
	-@echo "# 27  : csN-photon read                        : " ${PRODUCT27}    " o:" ${OBJECTS27}
	-@echo "# 28  : csN-photon total                       : " ${PRODUCT28}    " o:" ${OBJECTS28}
	-@echo "# 29a : stark                                  : " ${PRODUCT29a}   " o:" ${OBJECTS29a}
	-@echo "# 29b : shift                                  : " ${PRODUCT29b}   " o:" ${OBJECTS29b}
	-@echo "#" 
	-@echo "#" 
	-@echo "#             TDSE" 
	-@echo "#" 
	-@echo "#" 
	-@echo "# 30  tdse main   1e system                    : " ${PRODUCT30}    " o:" ${OBJECTS30}
	-@echo "# 30a tdse pes    1e system                    : " ${PRODUCT30a}   " o:" ${OBJECTS30a}
	-@echo "# 30b tdse hg     1e system                    : " ${PRODUCT30b}   " o:" ${OBJECTS30b}
	-@echo "# 30c tdse flux   1e system                    : " ${PRODUCT30c}   " o:" ${OBJECTS30c}
	-@echo "# 31  tdse(cos)   2e system                    : " ${PRODUCT31}    " o:" ${OBJECTS31}
	-@echo "# 32  tdse        2e system                    : " ${PRODUCT32}    " o:" ${OBJECTS32}
	-@echo "# 32a tdse(ramp)  2e system                    : " ${PRODUCT32a}   " o:" ${OBJECTS32a}
	-@echo "# 33  tdse pad                                 : " ${PRODUCT33}    " o:" ${OBJECTS33}
	-@echo "# 34  tdse read dipoles      (aux)             : " ${PRODUCT34}    " o:" ${OBJECTS34}
	-@echo "# 35  tdse read coefficients (aux)             : " ${PRODUCT35}    " o:" ${OBJECTS35}
	-@echo "# 36  tdse plotting 2d                         : " ${PRODUCT36}    " o:" ${OBJECTS36}

info:
	-@echo "# ==================================================="
	-@echo "# "
	-@echo "# "
	-@echo "#  Package fortran-amo:                              "
	-@echo "#  Source programs 1993-2013 for 1e/2e atomic systems" 
	-@echo "#  contains:                                         " 
	-@echo "#  h1e/h2e(fxd,free)/lopt/tdse                       " 
	-@echo "#" 
	-@echo "#              Version xxx first compiled 28032005UOP"
	-@echo "#" 
	-@echo "#" 
	-@echo "#                  1993/uoc/iesl/uop/ifa/qub/dcu/2013" 
	-@echo "#" 
	-@echo "#"  
	-@echo "#             maintened by:" ${MY_NAME}
	-@echo "#                  e-mail :" ${MY_EMAIL}
	-@echo "#                institute:" ${MY_INSTITUTE}
	-@echo "#                 homepage:" ${MY_HOMEPAGE}
	-@echo "#"  
	-@echo "#-----------------------------------------------------"
	-@echo "#"
	-@echo "#                     Date:" `date`
	-@echo "#                 hostname:" `hostname`
	-@echo "#                  Machine:" `uname -a`
	-@echo "#                 username:" ${LOGNAME}
	-@echo "#                directory:" ${PWD}
	-@echo "#-----------------------------------------------------"
	-@echo "#         vendor  compiler: ${CMP}"
	-@echo "# Loading Fortran compiler: ${FC},${F90}"
	-@echo "#         Fortran flags   : ${F90FLAGS},${FCFLAGS},${F95FLAGS}"
	-@echo "# Loading C/C++   compiler: ${CC},${C}"
	-@echo "#         C/C++   flags   : ${CXXFLAGS},${CFLAGS}"
	-@echo "#-----------------------------------------------------"
	-@echo "# Using     include  paths: ${INCLUDE}"
	-@echo "#"
	-@echo "# Loading        libraries:"
	-@echo "#                           ${LINK_SLATEC}"
	-@echo "#                           ${LINK_LAPACK}"
	-@echo "#                           ${LINK_BLAS}"
	-@echo "#                           ${LINK_NAG17}"
	-@echo "#                           ${LINK_NETCDF_F}"
	-@echo "#                           ${LINK_NETCDF_C}"
	-@echo "#                           ${LINK_ODE}"
	-@echo "#                           ${LINK_GSL}"
	-@echo "# ==================================================="


cleanbin:
	for f in $(PRODUCT);                       \
	do                                         \
	(if test -e $$f; then                       \
	(echo "Makefile:: Removing $$f" ; rm $$f ;) \
	fi;)                                        \
	done

cleanfxd:
	rm -f *.o *.mod \
	for f in $(FXD_EXE);                        \
	do                                          \
	(if test -e $$f; then                       \
	(echo "Makefile:: Removing $$f" ; rm $$f ;) \
	fi;)                                        \
	done

cleanfree:
	rm -f *.o *.mod     \
	rm -f *.o *.mod \
	for f in $(FXD_EXE);                        \
	do                                          \
	(if test -e $$f; then                       \
	(echo "Makefile:: Removing $$f" ; rm $$f ;) \
	fi;)                                        \
	done

cleanlopt:
	rm -f *.o *.mod     \
	for f in $(LOPT_EXE);                        \
	do                                          \
	(if test -e $$f; then                       \
	(echo "Makefile:: Removing $$f" ; rm $$f ;) \
	fi;)                                        \
	done

cleantdse:
	@$(foreach f,$(TDSE_EXE), $(call clean_files, $(f)))
#	$(foreach f,$(TDSE_EXE), rm ${BIN}/$(f);)
#	rm -f *.o *.mod     \
#	for f in $(TDSE_EXE); do \
	echo $$f
#	done
#	if test -e ${BIN}/$$f; then (echo "Makefile:: Removing $$f" ; rm ${BIN}/$$f)  fi ; \

clean:  
	rm -f ${SRC}/*.o ${SRC}/*.mod
	rm -f ${LIB}/libbasis.a
	# rm -f ${LIB}/libnag.a

pclean:
	rm -f ${SRC}/*.mpo ${SRC}/*.mod  

define clean_files
#	ifeq($(wildcard $(1)),)
#	-@echo "clean_files: $1 not present" ; 
	$(shell ls "${BIN}/$1")
#	else
#	(shell ls ${BIN}/$1) 
#-@echo "clean_files: $1 removed"
###	endif	
endef	
