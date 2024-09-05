!!!xxxxoexxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx     
!                                         written by xian tang, jan 1990
!                                          changes made by  hr, feb 1990
!   changes made 3/31/90 by hr,xtang,jzhang,laan:
!   changed to remove the rapid  oscillating part of the matrix
!   pulse shape changed to gaussian
!   pulse duration changed from -0.5* tau to 0.5*tau
!
!   this program uses the nag routine d02baf to integrate the diff. equation
!   and it can be used for restarting an old job with a unit 33, with time and
!   vector information
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!
! propagates the coefficient vector  C(t)
!
! y(t) expanded on a eigenstate basis: 

! y(t) = Sum_(nl) C_(nl) * y_(nl)
!
! H_0 * y_nl = E_nl y_nl,     <y_nl|y_n'l> = d_nn'
!
!
! [ H_0 + V(t) ] y(t) = i dy/dt ==> dC/dt = |H+V(t)| * C(t)
!
! input: tinp/tdse_bs.inp    (some integration parameters)
!        tinp/pulse.inp      (pulse parameters)       
!       
!         dat/heLL+1.nc      (field-free data (E_nl, V(nl,n'l+1) in netCDF format
!        or       
!         dat/dmx2e-v.LL+1.dat (as above in fortran binary)
!
!

PROGRAM tdse_bs_fxd_2e
  !
  USE PRECISION
  USE units
  USE parameter_tdse_fxd
  USE deriv_tdse_fxd
  USE io
  USE utils, ONLY:tafile
  USE pulse
!  use netcdf
!  USE ncFile
  USE DynamicIntegerArray

  !
  IMPLICIT NONE
  !
  INTEGER,                     PARAMETER :: nfft  = 16384
  !
  !
  REAL(dpk), ALLOCATABLE, DIMENSION(:)   ::  y
  REAL(dpk), ALLOCATABLE, DIMENSION(:)   ::  yp  
  !field! 
  INTEGER                                :: nout_T 
  !
  REAL(dpk)                              :: t, t1, tim, ti
  REAL(dpk)                              :: pop
  REAL(dpk)                              :: pop_g, pop_b, pop_k, pop_l
  !pes!
  CHARACTER(len=25)                      :: pes_file
  INTEGER                                :: npes
  INTEGER                                :: ntot_l, ne
  !
  INTEGER                                :: i, k, k1, nb, iu, kt,lk
  INTEGER                                :: nout1, nout2, nt, li, l, ikt, ik, ii, ll
  !hohg!
  REAL(dpk), ALLOCATABLE, DIMENSION(:)     :: dpr, dpi
  REAL(dpk), ALLOCATABLE, DIMENSION(:)     :: time 
  REAL(dpk), ALLOCATABLE, DIMENSION(:)     :: rt
  !REAL(dpk), ALLOCATABLE, DIMENSION(:)     :: at 
  REAL(dpk), ALLOCATABLE, DIMENSION(:)     :: vt
  CHARACTER(len=6)                         :: form         

  REAL(dpk), ALLOCATABLE, DIMENSION(:)     :: tm 
  INTEGER                                  :: tSteps 
  !
  !nag
  REAL(dpk), ALLOCATABLE, DIMENSION(:,:)   :: work       !nag, work array
  INTEGER                                  :: ifail     !nag/d02baf
  !io
  CHARACTER(LEN=25)                      ::    coe_file     !o
  CHARACTER(LEN=25)                      ::    ion_file     !o
  CHARACTER(LEN=25)                      ::    pop_file     !o
  !
  INTEGER                                :: nion, npop, ncoe
  !ncFile
!  INTEGER                                :: ncid 
  !
  LOGICAL                                :: coe_file_exists
  !
  INTEGER                                :: dim_t 
  !
  !nag
  EXTERNAL                               d02baf        ! nag
  EXTERNAL                               tdse_v 
  !exe!
  
  !
  !  CALL getarg(1, argv)             ! nof partial waves
  !  READ(argv,*)   lmax              !
  
  ! define i/o id's and file names

  nion  = 10
  npop  = 11
  ncoe  = 12 
  npes  = 16

  !in
!  atomic_file = "dmx2ebb"      
  !out
  ion_file='tdat/pgt.dat'
  pop_file='tdat/pop.dat'
  coe_file='tdat/coe.dat'
  pes_file='tdat/pesl.dat'

  
  ! now read
  CALL input_tdse_fxd              ! tinp/tdse_bs_fxd_2e.inp
  CALL output_tdse_fxd             ! tout/tdse_bs_fxd_2e.out
  CALL make_space                  ! allocate space for arrays en,dmx
  CALL read_system(gauge)          ! f90 (dat/systemLL+1gauge.dat)
  CALL read_field                  ! read field parameters (tinp/pulse.inp)
  CALL save_field                  !  out/pulse.out  !


  
  ti     = tau * start_time !  tf     = +tau * start_time

  
  !  OPEN(20,FILE='tout/tdse.out') 
  WRITE(*,*)'#' 
  WRITE(*,*)'#                                  light:' 
  WRITE(*,*)'#' 
  WRITE(*,*)'#        peak electric field       Eo  = ', e0,        ' a.u.'
  WRITE(*,*)'#             photon energy        Wph = ', w0 ,       ' a.u.'
  WRITE(*,*)'#             pulse duration       tau = ', tau,       ' a.u.' 
  WRITE(*,*)'#             pulse type     pulseType = ', pulseType 
  WRITE(*,*)'#             peak intensity       Io  = ', e0**2,     ' a.u.'
  WRITE(*,*)'#             field period       tau0  = ', tau0,      ' a.u.' 
  WRITE(*,*)'#             initial time         ti  = ', ti,        ' a.u.' 
  WRITE(*,*)'#                                  tau = ', tau,       ' a.u.' 
  WRITE(*,*)'#' 
  WRITE(*,*)'#                                   atom:' 
  WRITE(*,*)'# ionic ground  state   energy    E(+) = ', en_ion_1,  ' a.u.'
  WRITE(*,*)'#       ground  state   energy    E_g  = ', en_ground, ' a.u.'
  WRITE(*,*)'#       maximum kinetic energy  En_cut = ', en_cut,    ' a.u.'
  WRITE(*,*)'#' 
  WRITE(*,*)'#                             interaction' 
  WRITE(*,*)'#' 
  WRITE(*,*)'#      gauge interaction         gauge = ', gauge 
  !    

  
 !coefficients file
  OPEN(ncoe,file=coe_file,form='unformatted',access='sequential') ! coef(t) file
  WRITE(ncoe) e0, w0, tau, lmax, en_cut

  !find ntot
  ntot = 0
  change_energy_axis: DO  l = 1, lmax + 1 ! reset zero   ! en(ne, l) > 0  continuum
                                                       !    en(ne, l) < 0  bound
     iu = 0          
     exclude_high_energies: DO  ne = 1, n(l)

        IF(atomic_name=="he") THEN
           en(ne,l) = 0.5_dpk * en(ne,l) - en_ion_1         ! (in a.u.) !standard 2e-data
        ENDIF

        IF(en(ne,l).GT.en_cut) CYCLE                     ! drop  E > en_cut
        iu = iu + 1
     ENDDO exclude_high_energies
     
     n(l) = iu
     
     ntot = ntot + n(l)            ! total nof states (actual size of psi(t))
     
   
     WRITE(ncoe) n(l), ( en(ne,l), ne = 1, n(l))   
     
     WRITE(*,*)'#'
     WRITE(*,*)'#      partial wave     l = ', l - 1 
     WRITE(*,*)'#'
     
  ENDDO change_energy_axis

  
  WRITE(*,*)    '# tdse_bs::                              Lmax = ', lmax
  WRITE(*,*)    '# tdse_bs::                              Emax = ', en_cut
  WRITE(*,*)    '# tdse_bs::                  nof states  n(l) = ', n
  WRITE(*, *)   '# tdse_bs::            total nof states n_tot = ', ntot

  
  WRITE(ncoe) ntot
  
  
  neq = 2*ntot

  ALLOCATE(  y(neq))
  ALLOCATE( yp(neq))

  
  y    = 0.0_dpk 
  y(1) = 1.0_dpk
  t    = ti

 
  ! used in the dgemv as well 
  nsum(1) = 1
  DO i = 2, lmax+1
     nsum(i) = nsum(i-1) + n(i-1)
  END DO
  
  
  
  !
  !  Make the diagonal part of matrix  E(nl,nl)
  !

  nb = 0
  assign_diagonal_hamiltonian:   DO   k = 1, lmax+1
     loop_over_states_for_each_l:DO  k1 = 1, n(k)

        nb      = nb + 1
        dag(nb) = en(k1,k)              ! a.u.

     ENDDO loop_over_states_for_each_l
  ENDDO assign_diagonal_hamiltonian

  ! set by hand

  dag(1) = en_ground - en_ion_1

  WRITE(*,*)    '#tdse_bs::                                 E+ = ', en_ion_1,  ' a.u. ', en_ion_1*27.211
  WRITE(*,*)    '#tdse_bs::                                E++ = ', en_ground, ' a.u. ', en_ground*27.211
  WRITE(*,*)    '#tdse_bs::                           E++ - E+ = ', dag(1),    ' a.u. ', dag(1)*27.211

  !open files
  OPEN(nion,file=ion_file)          ! ionization file
  
  DO i = 1, lmax+1
     CALL tafile(i,i-1,"pop")       ! partial population files !pop0.dat, pop1.dat,...
  ENDDO

  OPEN(npop,file=pop_file)          ! population   file


  WRITE(*,'(a15,2x,a15)') 't','norm'

  ! initial conditions 


  dim_t = 2 * ABS(INT(ti/tau_i(1)))
  
  pop_g = 0.0_dpk
  pop_b = 0.0_dpk
  pop_k = 0.0_dpk
 
  tSteps = 10


  WRITE(*,*)" propagation times: ti = ", ti,  ti*t_fs
  WRITE(*,*)"                    tf = ", dim_t*tau_i(1), ABS(ti)
  WRITE(*,*)"           steps dim_t = ", dim_t
  WRITE(*,*)"                    dt = ", tau_i(1)/tsteps, (tau_i(1)/tsteps)*t_fs


  
  ALLOCATE( yderiv(neq)  )            ! dy/dt
  ALLOCATE(   work(neq, 7) )          ! work array for nag

  
  kt     = 0
  time_loop:DO  l = 1, dim_t 

     
     nout1 = kt + 1
     time_loop_for_hhg: DO  lk = 1, tSteps ! more steps to monitor

        kt  = kt + 1 
        tim = t + DBLE(tau_i(1)/tsteps)

!        WRITE(30,*) tim, a_t(tim)       
        WRITE(*,*)"lk, = ", lk
        WRITE(*,*)"t, tim = ", t, tim
        
        neval = 0
        tol   = 1.0D-10
        ifail = 0

        CALL d02baf(t, tim, neq, y, tol, tdse_v, work, ifail)

        WRITE(*,*)" t, tim, = ", t, tim
        

        
        ! ground state population
        
        pop_g = y(1)**2 + y(ntot+1)**2
        
        ! bound state population
        
        pop_b = 0.0_dpk
        nt = 0
        bound_population_t: DO  k = 1, lmax+1
           DO  k1 = 1, n(k)
              get_bound_states:IF( en(k1,k).LE.0.0_dpk) THEN
                 pop_b = pop_b + ( y(k1 + nt)**2 + y(ntot+nt+k1)**2 )
              ENDIF get_bound_states
           ENDDO
           nt = nt + n(k)
        ENDDO bound_population_t

        ! ionization
        pop_k = 1.0_dpk - pop_b

        !save
        WRITE(nion,'(5E25.14)') tim,  pop_g, pop_b, pop_k, 1-pop_g  ! pgt.dat


        
        !
        ! partial population
        !
        
        nt = 0
        partial_population:DO  k = 1, lmax+1
           pop_b = 0.0_dpk
           partial_bound_population:DO  k1 = 1, n(k)
              get_bound_states_L:IF(en(k1,k).LE.0.0_dpk) THEN
                 pop_b = pop_b + ( y(k1 + nt)**2 + y( ntot+nt+k1)**2 )
              ENDIF get_bound_states_L
           ENDDO partial_bound_population
           
           !popL.dat
           WRITE(k,'(6e25.14)') tim, pop_b, ( y(k1+nt)**2 + y(ntot+nt+k1)**2 , k1=1,4)
           nt = nt + n(k)

        ENDDO partial_population
        !     t   = tim
     ENDDO time_loop_for_hhg


     WRITE(*,*) '# cycle = ', l, tim

     
     WRITE(ncoe) tim, ( y(ii), ii = 1, neq)

     !  write population for partial waves

     WRITE(npop,'(i8, 1X,e15.6)') kt, tim

     pop = 0.0_dpk
     nt = 0
     population_t: DO  k = 1, lmax+1
             
        !  |Y(nL)|^2  , n = 1-5, L = 0, Lmax

        pop_l = 0.0_dpk
        DO  k1 = 1, n(k)
           pop_l = pop_l + y(k1 + nt)**2 + y( ntot + nt + k1 )**2
        END DO
        
        pop = pop + pop_l

        !pop.dat        
        WRITE(npop,'(I6,1X,5e25.14)') k-1, pop_l, ( y(k1+nt)**2 + y(ntot+nt+k1)**2 , k1=1,4)

        nt = nt + n(k)
        
     ENDDO population_t

     WRITE(*,*) tim, pop

     !          WRITE(npop,'(/)')

     nt = 0
     pop = 0.0_dpk
     

     !pesL.dat
     OPEN(npes,file=pes_file)
     WRITE(*,*) '# pes for partial waves l = 0 - ', lmax+1
     WRITE(*,*) '#                     stored in ', pes_file
     WRITE(npes,'(a1,1x,2i6)') '&', lmax+1, ntot
     ntot_l = 0
     DO  ll = 0, lmax
        WRITE(npes,'(a1,1x,2i6)') '&',ll, ntot_l   ! state 'n' of partial wave 'l'
        DO  ne = 1, n(ll+1)
           WRITE(npes,'(4e14.6)')& 
                & en( ne, ll+1)                                    , &
                &  y( ne + ntot_l )**2 + y(ne + ntot_l + ntot)**2 , &
                &  y( ne + ntot_l )                               , & 
                &  y( ne + ntot_l + ntot )
        ENDDO
        ntot_l = ntot_l + n(ll+1)
     ENDDO
     CLOSE(npes)
     WRITE(*,*)'# total nof states             ntot = ', ntot
     WRITE(*,*)'# total nof equations           neq = ', neq
     

  ENDDO time_loop
       
  CLOSE(ncoe)
  CLOSE(npop)

!!%
!store for spectra analysis

       
  OPEN(npes,file=pes_file)
  WRITE(*,*) '# pes for partial waves l = 0 - ', lmax+1
  WRITE(*,*) '#                     stored in ', pes_file

  
  WRITE(npes,'(a1,1x,2i6)') '&', lmax+1, ntot
       ntot_l = 0
       partial_wave_l:DO  l = 0, lmax
          
          WRITE(npes,'(a1,1x,2i6)') '&',l, ntot_l   ! state 'n' of partial wave 'l'
          
          eigenstate_ne:DO  ne = 1, n(l+1)
             WRITE(npes,'(4e14.6)')& 
                  & en( ne, l+1)                                    , &
                  &  y( ne + ntot_l )**2 + y(ne + ntot_l + ntot)**2 , &
                  &  y( ne + ntot_l )                               , & 
                  &  y( ne + ntot_l + ntot )
          ENDDO eigenstate_ne
          
          ntot_l = ntot_l + n(l+1)
          
       ENDDO partial_wave_l
       
       CLOSE(npes)
       
!!%       

       
     END PROGRAM tdse_bs_fxd_2e
            
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!EOF
