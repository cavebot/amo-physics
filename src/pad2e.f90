!---22/03/2021---
! Program calculates the 2-e angular Distribution
! for specififc energy E, and specific energy sharing e1,e2 between ejected electrons
! run as ./bin/helium_ad2e_Ee1e2  -fieldcycle
!
! fieldcycle refers to the time of evaluation of the distribution e.g the 12th cycle
! of a 12 cycle field is the end of the pulse
!


  ! S( Omega_1, Omega_2)
  !
  !
  ! P(Omega_1,Omega_2) = S_{n,L} S_{l1,l2} C_{n,L} * V_{n1l1;n2l2} * A(n1l1;n2l2) A_{12}{ Y_{L;l1l2}(Omega_1,Omega_2) }
  !
  ! 
  ! A2e_{12}{ Y_{L;l1l2}(Omega_1,Omega_2) } =
  ! S_{m1 m2}{ C_{L,l1,l2;m1,m2,-M_L} ( Y^*_{l1m1}(Omega_1) * Y^*_{l2m2}(Omega_2) + (-)^Ph * Y^*_{l1m1}(Omega_2)*Y^*_{l2m2}(Omega_2) ) }
  !
  ! Ph = l1 + l2 + L 
  !
  ! A(n1l1;n2l2) = (-i)^{l1 + l2} e^{ i(delta_{n1l1} + delta_{n2l2} } = 
  !
  !


 
  ! Due to the assumed field's linear polarization along the Z-axis
  ! the magnetic quantum numbers are fixed to zero  m_1 = m_2 = 0
  ! (then automatically from C_{L,l1,l2;m1,m2,-ML) --> M_L = 0 ) and remains
  ! C_{Ll1l2;000)

PROGRAM pad2e

  USE PRECISION
  USE io,             ONLY:nout, ninp
  USE units,          ONLY: m_pi, zi
  USE param,          ONLY: rmax                 
  USE bs_frb_2e,      ONLY: read_target_energies
  USE atom_2e                                      !Provides routines for reading CI/TDSE coefficients
  USE parameter_tdse_fxd
  USE pad_utils
  USE anglib                                       !clebsch-gordon Coefficients
  USE spherical_harmonics                          !spherical harmonics
  USE core_phase_shift
  
  IMPLICIT NONE
  !
  REAL(DPK)                                      :: dummie1
  INTEGER                                        :: ll1,ll2, nn1, nn2
  INTEGER                                        :: ne_1e, nl_1e
  INTEGER                                        :: l1e_max
  !
  REAL(dpk), DIMENSION(:,:), POINTER             :: en1e            !nl,ns
  REAL(dpk), ALLOCATABLE, DIMENSION(:,:)         :: e1e             !nl,ns
  INTEGER,   ALLOCATABLE, DIMENSION(:)           :: ndi_l           ! pop with  E(ndi_l,l) = E++
  INTEGER,   ALLOCATABLE, DIMENSION(:)           :: nsi_l           ! pop with  E(ndi_l,l) = E+
  INTEGER                                        :: n_l2e           ! nof CI states of symmetry l2e
  
  !Redundant variables now after reformulating loops. Fix this.
  !  INTEGER                                        :: i,j,k
  INTEGER                                        :: l, ic, ie, ith1, ith2   
  INTEGER                                        :: nof_theta_1, nof_theta_2
  !Wavefunction variables
  TYPE(symmetry_ls2e), ALLOCATABLE, DIMENSION(:) :: w2e
  COMPLEX(DPK), DIMENSION(:,:), ALLOCATABLE      :: A2e              !bipolar spherical harmonic
  !
  INTEGER                                        :: m1, m2
  REAL(DPK)                                      :: phi_1, phi_2
  REAL(DPK),      DIMENSION(:),  ALLOCATABLE     :: vtheta_1, vtheta_2
  !! cfgs matching (L, n1l1;n2l2) given (l_int, e1_int,l1_int;e2_int,l2_int)
  INTEGER,        DIMENSION(:),  ALLOCATABLE     :: icf, nc1, lc1, nc2, lc2
  COMPLEX(dpk),   DIMENSION(:),  ALLOCATABLE     :: a12
  COMPLEX(DPK)                                   :: ph12
  COMPLEX(DPK), DIMENSION(:), ALLOCATABLE        :: cv_t               ! cv_t = S_n c_{nL}(t) * V_{nL;n1l1,n2l2}
  !
  !command line arguments
  REAL(dpk)                                      :: e1_ic, e2_ic, e1_int, e2_int, de
  !For phase shifts
  REAL(DPK), DIMENSION(:,:), ALLOCATABLE         :: k_l, d_lk   !k_l, delta_kl array for phase shift values
  REAL(dpk)                                      :: del_kr
  ! Use this instead.
  REAL(dpk)                                      :: k1, k2
  COMPLEX(dpk)                                   :: ylm1, ylm2, ylm1x,ylm2x
    COMPLEX(dpk)                                 :: clb
  CHARACTER(len=100)                             :: A2e_outfile
  !input variables
  CHARACTER(len=100)                             :: partial_l1, partial_l2, partial_l, num_cycles
  INTEGER                                        :: l1_int, l2_int, l_int, num_cycles_int
                                                !exe

  

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!calculate angular distribution
  !
  !Testing clebsch-gordon coefficients and spherical harmonic routines.
  WRITE(*,*) "#   C-G <l1,m1; l2, m2|L, M >,  <l1, m1; l2, m2| L, M >/sqrt(2L+1),  = "
  WRITE(*,*) "# L = 0"
  WRITE(*,*) "#                  0 0; 0 0; 0 0 = ", cleb(2*0, 0, 2*0, 0, 2*0, 0), c3j(2*0, 0, 2*0, 0, 2*0, 0)
  WRITE(*,*) "#                  1 0; 1 0; 0 0 = ", cleb(2*1, 0, 2*1, 0, 2*0, 0), c3j(2*1, 0, 2*1, 0, 2*0, 0) 
  WRITE(*,*) "#                  2 0; 2 0; 0 0 = ", cleb(2*2, 0, 2*2, 0, 2*0, 0), c3j(2*2, 0, 2*2, 0, 2*0, 0)
  WRITE(*,*) "#                  3 0; 3 0; 0 0 = ", cleb(2*3, 0, 2*3, 0, 2*0, 0), c3j(2*3, 0, 2*3, 0, 2*0, 0)
  WRITE(*,*) "#                  4 0; 4 0; 0 0 = ", cleb(2*4, 0, 2*4, 0, 2*0, 0), c3j(2*4, 0, 2*4, 0, 2*0, 0)
  WRITE(*,*) "# L = 2"
  WRITE(*,*) "#                  1 0; 1 0; 0 0 = ", cleb(2*1, 0, 2*1, 0, 2*2, 0)/SQRT(5.0_dpk), c3j(2*1, 0, 2*1, 0, 2*2, 0) 
  WRITE(*,*) "#                  2 0; 2 0; 0 0 = ", cleb(2*2, 0, 2*2, 0, 2*2, 0)/SQRT(5.0_dpk), c3j(2*2, 0, 2*2, 0, 2*2, 0)
  WRITE(*,*) "#                  3 0; 3 0; 0 0 = ", cleb(2*3, 0, 2*3, 0, 2*2, 0)/SQRT(5.0_dpk), c3j(2*3, 0, 2*3, 0, 2*2, 0)
  WRITE(*,*) "#                  4 0; 4 0; 2 0 = ", cleb(2*4, 0, 2*4, 0, 2*2, 0)/SQRT(5.0_dpk), c3j(2*4, 0, 2*4, 0, 2*2, 0)
  WRITE(*,*) "#                  0 0; 2 0; 2 0 = ", cleb(2*0, 0, 2*2, 0, 2*2, 0)/SQRT(5.0_dpk), c3j(2*0, 0, 2*2, 0, 2*2, 0)
  WRITE(*,*) "#                  1 0; 1 0; 2 0 = ", cleb(2*1, 0, 2*1, 0, 2*2, 0)/SQRT(5.0_dpk), c3j(2*1, 0, 2*1, 0, 2*2, 0)
  WRITE(*,*) "#                  1 0; 3 0; 2 0 = ", cleb(2*1, 0, 2*3, 0, 2*2, 0)/SQRT(5.0_dpk), c3j(2*1, 0, 2*3, 0, 2*2, 0)
  WRITE(*,*) "#                  2 0; 2 0; 2 0 = ", cleb(2*2, 0, 2*2, 0, 2*2, 0)/SQRT(5.0_dpk), c3j(2*2, 0, 2*2, 0, 2*2, 0)
  WRITE(*,*) "#                  2 0; 4 0; 2 0 = ", cleb(2*2, 0, 2*4, 0, 2*2, 0)/SQRT(5.0_dpk), c3j(2*2, 0, 2*4, 0, 2*2, 0)


  
  
    !    write(*,*) "sph-harm l, m = ", sphharm(1, -1, (m_pi/2), 0.0_dpk)

  !Working


  
  !get cmd arguments
  !    call getarg(1, L_total)
  CALL getarg(1, partial_l1)
  CALL getarg(2, partial_l2)
  CALL getarg(3,  partial_l)
  CALL getarg(4, num_cycles)
  !
  READ(partial_l1(1:1), "(i1)")              l1_int
  READ(partial_l2(1:1), "(i1)")              l2_int
  READ(partial_l(1:1),  "(i1)")              l_int
  READ(num_cycles(1:2), "(i2)")              num_cycles_int
  !
    

  !WRITE(*,*) "#            (L; l1, l2) = ", l, l1_int, l2_int
  !WRITE(*,*) "# Field cycle  number: c = ",  num_cycles_int 
  
    
  e1_int = 0.15
  e2_int = 0.23

  
  !----------------------------Get number of states 1e from inp/h1e.inp -------------------------
    
  OPEN(unit = ninp, file = "inp/h1e.inp", status = 'old')
  READ(ninp, *)
  READ(ninp, *)
  READ(ninp, *)
  READ(ninp, *)  dummie1, rmax, dummie1          !* box-radius *
  CLOSE(ninp)

  !--------------------------R￼ead TDSE coeffs and CI coeffs and configuration data-------------------------

  CALL input_tdse_fxd                                  ! tinp/tdse_bs_fxd_2e.inp
  CALL output_tdse_fxd                                 ! tout/tdse_bs_fxd_2e.out
  CALL read_target_energies(en1e)                      ! read 1e energies (in Ryd)
  CALL read_w2e_ct(w2e,lmax,num_cycles_int)            ! read tdse wf2e
  DO l = 0, lmax                                       ! 
     CALL read_w2e_ci(w2e(l),l)                        !reads coeffs   Phi^{NL}_{a} = \sum_a C^{NL}_a Phi^{(0)}_{L;a}
  END DO                                               !                         a  = (nl;n'l')

  WRITE(*,*) "#                                                L = ", l
  WRITE(*,*) "#                                  box size   rmax = ", rmax      
  WRITE(*,*) "#                                      field cycle = ", num_cycles_int
  WRITE(*,*) "#"
  WRITE(*,*) "#                    output places in pad2e/pad2e.out"  
  
  !1-e energy matrix initialisations

  en1e = en1e * 0.5_dpk              ! convert energies in a.u.
  nl_1e = SIZE(en1e,dim=1)           ! e1e(l1_max, n1_max)
  ne_1e = SIZE(en1e,dim=2)           ! e1e(l1_max, n1_max)
  
  ! for conformity reasons we rewrite
  ! en1e(1:nl_1e,1:ne_1e) --> e1e(0:nl_1e-1,1:ne_1e) -->

  !First find the lread_target_energiesargest angular momentum included in the cfg channels.
  
  l1e_max = 0
  DO l = 0, lmax
     IF(SIZE(w2e(l)%l12,dim=1).GT.l1e_max) l1e_max = SIZE(w2e(l)%l12,dim=1)
  ENDDO
  l1e_max = l1e_max - 1   ! since l1e_max inside the loop returns the size of l12
  
  PRINT*,"#                                 l1e_max = ", l1e_max
  IF((nl_1e-1).LT.l1e_max) THEN
     PRINT*,"#                      something wrong here: "
     PRINT*,"#    max l1e calculated              ne_1e = ", nl_1e-1
     PRINT*,"#    max l1e included in cfg files l1e_max = ", l1e_max
     STOP
  ENDIF
  
  
  ALLOCATE( e1e(0:l1e_max,1:ne_1e))
  
  DO l = 0, l1e_max
     e1e(l,:) = en1e(l+1,:)
  ENDDO
  
      
  DEALLOCATE(en1e)
  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Calculate SI, DI
  
  ALLOCATE( ndi_l(0:lmax) )
  ALLOCATE( nsi_l(0:lmax) )
  
      
  ndi_l  =  0
  nsi_l  =  0
  find_si_di_thresholds:DO l = 0, lmax
     
     n_l2e = w2e(l)%net
     
     SI_THRESHOLD:DO  ie = 1, n_l2e     ! find SI threshold energy  for symmetry L
        nsi_l(l) = ie
        IF(  w2e(l)%e2e(ie) > en_ion_1 ) EXIT
     ENDDO SI_THRESHOLD
     

     DI_THRESHOLD:DO  ie = 1, n_l2e     ! find DI threshold energy  for symmetry L
        ndi_l(l) = ie
        IF( ( w2e(l)%e2e(ie) ) > 0.0_dpk ) EXIT
     ENDDO DI_THRESHOLD


     WRITE(*,*) "# SI threshold                            en_ion_1 = ", en_ion_1
     WRITE(*,*) "# SI threshold                E(", nsi_l(l), l, ") = ", w2e(l)%e2e(nsi_l(l))
     WRITE(*,*) "# DI threshold                E(", ndi_l(l), l, ") = ", w2e(l)%e2e(ndi_l(l))
     WRITE(*,*) "# Max energy                  E(", n_l2e, l,    ") = ", w2e(l)%e2e(n_l2e)

         
  ENDDO find_si_di_thresholds


  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! calculate phase shifts


      
  ALLOCATE(d_lk(0:l1e_max, 1:ne_1e))  !phase shifts 
  ALLOCATE( k_l(0:l1e_max, 1:ne_1e))  !wavenumber

  
  k_l  = 0.0_dpk
  d_lk = 0.0_dpk
  
  k_l  = SQRT( 2.0_dpk * ABS(e1e) )
  
  d_lk = k_l * rmax + (2.0_dpk/k_l) * LOG(2.0_dpk*k_l*rmax)
  !
  loop_l1e: DO l = 0, l1e_max
     d_lk(l,:) = d_lk(l,:) - 0.5_dpk * l * m_pi 
  ENDDO loop_l1e
  




  ! define the angular grid

  nof_theta_1 = 1                ! this is when the one of the electrons is at fixed angle
  nof_theta_2 = 360              ! angle for the other electron


  WRITE(*,*) "#         size of angular grid points nof_theta_1  =", nof_theta_1
  WRITE(*,*) "#         size of angular grid points nof_theta_2  =", nof_theta_2

  
  ALLOCATE( vtheta_1(1:nof_theta_1))
  ALLOCATE( vtheta_2(1:nof_theta_2))

  
  phi_1   = 0.0_dpk
  phi_2   = 0.0_dpk
  

  DO ith1 = 1, nof_theta_1
     vtheta_1(ith1) = (ith1-1)* m_pi/nof_theta_1
  ENDDO
  
  DO ith2 = 1, nof_theta_2
     vtheta_2(ith2) = (ith2-1)* m_pi/nof_theta_2
  ENDDO
  
  ! The electron's energies should be fixed as well. In the below  e1 and E are fixed)

  




  !
  ! find the configuration channels consistent with the | k_1,k_2 > asymptotic wf2e
  !
  
     l = l_int
     de = 0.018   !0.5_dpk * ABS(e1e(ll1,nn1+1)-e1e(ll1,nn1-1))
     

     ALLOCATE(icf(0))
     ALLOCATE(a12(0))
     ALLOCATE(nc1(0))
     ALLOCATE(lc1(0))
     ALLOCATE(nc2(0))
     ALLOCATE(lc2(0))

     ph12 = 0
     
     nn1 = 0
     nn2 = 0
     ll1 = 0
     ll2 = 0
     
     select_cfg_in_L: DO ic = 1, w2e(l)%ncf                   ! Sum over all cfs (for L)

        !short-cut definitions
        nn1 = w2e(l)%n1(ic)        ! n1
        ll1 = w2e(l)%l1(ic)        ! l1
        !
        nn2 = w2e(l)%n2(ic)        ! n2
        ll2 = w2e(l)%l2(ic)        ! l2


        e1_ic = e1e(ll1,nn1)
        e2_ic = e1e(ll1,nn2)
        
        ! exclude any (n1l1;n2l2) that does not match the selection criteria
        
        IF(e1_ic < 0) CYCLE
        IF(e2_ic < 0) CYCLE
        
        IF( (ABS(e1_ic - e1_int) < ABS(de)).AND.( ABS(e2_ic-e2_int) < ABS(de)) ) THEN

           !           IF( (ll1 == l1_int).AND.(ll2 == l2_int) )  THEN
           !IF(ll1==ll2)  THEN 
           
              nc1  = [nc1, nn1]
              nc2  = [nc2, nn2]
              lc1  = [lc1, ll1]
              lc2  = [lc2, ll2]              
              icf  = [icf, ic]
              
              WRITE(*,*) "# SI threshold                            en_ion_1 = ", en_ion_1
              PRINT*, '#                                                 ic  = ', ic
              PRINT*, '#                                     ... ic ll1, ll2 = ', ic , nn1, ll1, nn2, ll2
              PRINT*, '#              (e2_ic - e1_int),  e1_ic,  e1_int,  de = ', ABS(e1_ic - e1_int), e1_ic, e1_int, ABS(de)
              PRINT*, '#              (e1_ic - e2_int),  e2_ic,  e2_int,  de = ', ABS(e2_ic - e2_int), e2_ic, e2_int, ABS(de)
              
              
              !short-cut definitions
              k1   = k_l(ll1, nn1)
              k2   = k_l(ll2, nn2)
              

              del_kr = (k1 + k2) * rmax  + (2.0_dpk/k1) * LOG( 2.0_dpk * k1 * rmax) + (2.0_dpk/k2) * LOG(2.0_dpk * k2 * rmax ) 
              ph12 = (-1)**((nn1 - 1 ) + ( nn2 - 1) )  * ( COS(del_kr) - zi * SIN(del_kr))
              
              
              !ph12 = (-1)**(nc1(ic) + nc2(ic)) * cdexp( - zi * ( d_lk(lc1(ic),nn1(ic) ) + d_lk(ll2(ic),nn2(ic)) )
              !                   * cdexp(-zi * ( (k1 + k2) * rmax  + (2/k1) * LOG( 2 * k1 * rmax) + (2/k2)*LOG(2*k2*rmax) ) )
              
              !              a12  = [a12, ph12]


              a12  = [a12, ph12]
              
              !IF(ll1 == ll2) THEN
               !  a12  = [a12, ph12]
                 !a12  = [a12, 0.5_dpk+zi*0.0_dpk]
                ! PRINT*, "# =================================== > nn1,ll1 = nn2, ll2", nn1,ll1, nn2,ll2, ph12
              !ELSE
                 !                 a12  = [a12, 0.0_dpk + zi*0.0_dpk]
               !  a12  = [a12, ph12]
              !ENDIF
              
           !ENDIF

        ELSE IF( (ABS(e2_ic - e1_int) < ABS(de)).AND.( ABS(e1_ic-e2_int) < ABS(de)) ) THEN



           !           IF( (ll1 == l1_int).AND.(ll2 == l2_int) )  THEN
           !IF(ll1==ll2)  THEN 
              
           
              nc1  = [nc1, nn1]
              nc2  = [nc2, nn2]
              lc1  = [lc1, ll1]
              lc2  = [lc2, ll2]              
              icf  = [icf, ic]
              

              k1   = k_l(ll1, nn1)
              k2   = k_l(ll2, nn2)

              del_kr = (k1 + k2) * rmax  + (2.0_dpk/k1) * LOG( 2.0_dpk * k1 * rmax) + (2.0_dpk/k2) * LOG(2.0_dpk*k2*rmax)
              
              ph12 = (-1)**(nn1 + nn2)  * ( COS(del_kr) - zi * SIN(del_kr) )
              
           
              !ph12 = (-1)**(nc1(ic) + nc2(ic)) * cdexp( - zi * ( d_lk(lc1(ic),nn1(ic) ) + d_lk(ll2(ic),nn2(ic)) )              
              !ph12 = (-1)**((nn1 + nn2) )  & 
              !     * cdexp(-zi * ( (k1 + k2) * rmax  + (2.0_dpk/k1) * LOG( 2 * k1 * rmax) + (2.0_dpk/k2)*LOG(2.0_dpk*k2*rmax) ) )  
              !              a12  = [a12, ph12]

              !              a12  = [a12, ph12]
              
!              IF(ll1 == ll2) THEN
                 a12  = [a12, ph12]
                 !                 a12  = [a12, 0.5_dpk+zi*0.0_dpk]
                 !                 a12  = [a12, 0.5*ph12]
                 !                 PRINT*, "# =================================== > nn1,ll1 = nn2, ll2", nn1,ll1, nn2,ll2, ph12
!              ELSE
                 !                 a12  = [a12, 0.0_dpk + zi*0.0_dpk]
!                 a12  = [a12, ph12]
!              ENDIF
              
              PRINT*, '# icx  = ', ic
              PRINT*, '# ... icx ll1x, ll2x = ', ic , nn1, ll1, nn2, ll2
              PRINT*, '#     (e2_ic - e1_int), e2_ic, e1_int = ', ABS(e1_ic - e1_int), e2_ic, e1_int, ABS(de)
              PRINT*, '#     (e1_ic - e2_int), e1_ic, e2_int = ', ABS(e1_ic - e2_int), e1_ic, e2_int, ABS(de)
              
           ENDIF
              
           !ENDIF

        
        ENDDO select_cfg_in_L

     PRINT*, '#========================================================================='

     
     ! ENDDO loop_cfg_L
     PRINT*, "------------------------------------------------------------------"
     PRINT*, icf
     DO ic = 1, SIZE(icf)
        PRINT*, icf(ic), nc1(ic), lc1(ic), nc2(ic), lc2(ic), a12(ic) 
     ENDDO


     !STOP
     
     !Now calculate,  cv_t = Sum_{n} c_nL(t) * v_nL_{n1l1;n2l2}  for the icf -> n1l1;n2l2 configurations

    
     ALLOCATE(cv_t(SIZE(icf)))

     cv_t = 0.0_dpk
     projection_channels: DO ic = 1, SIZE(icf)                                                                       
        loop_td_coefficients: DO ie = ndi_l(l), w2e(l)%net             ! DI 2e-states included in the psi(\r1,\r2,t) for 'L' symmetry
          
           cv_t(ic) = cv_t(ic) + w2e(l)%ct(ie) * w2e(l)%cv(ie,icf(ic))                      
        ENDDO loop_td_coefficients
     ENDDO projection_channels
     
     DO ic = 1, SIZE(icf)
        PRINT*, cv_t(ic)
     ENDDO
!     STOP
      

  !fix theta_1 and loop over theta_2 with fixed step


 ALLOCATE(  A2e(1:SIZE(vtheta_1),1:SIZE(vtheta_2)) ) 
 

 A2e = 0.0_dpk                                !depends on (nL;l1l2/(theta_1, theta_2))                  
 m1 = 0
 m2 = 0

  
  theta_1: DO ith1 = 1, SIZE(vtheta_1)
     theta_2: DO ith2 = 1, SIZE(vtheta_2)


        L2e: DO l = l_int, l_int                                 !Sum over total 'L2e'
           k1k2_channels: DO ic = 1, SIZE(icf)  !channels {L_int; n1_int, l1_int,n2_int, l2_int}
           
              !note: for ph1=ph2 = 0 ylm's are real not complex quantities   
              ylm1 = sphharm(   lc1(ic), m1, vtheta_1(ith1), 0.0_dpk)
              ylm2 = sphharm(   lc2(ic), m2, vtheta_2(ith2), 0.0_dpk)

              ylm1x = sphharm(   lc2(ic), m2, vtheta_1(ith1), 0.0_dpk)
              ylm2x = sphharm(   lc1(ic), m1, vtheta_2(ith2), 0.0_dpk)

              clb =     cleb( 2*lc1(ic), m1, 2*lc2(ic),  m2, 2*l, 0)

              !              IF(lc1(ic).EQ.lc2(ic)) THEN                 
                 !                 A2e(ith1, ith2) = A2e(ith1,ith2) + cv_t(ic) * a12(ic) * clb * (ylm1 * ylm2  + ylm1x * ylm2x )
                 
                 A2e(ith1, ith2) = A2e(ith1,ith2) + cv_t(ic) * a12(ic) * clb * ylm1 * ylm2
                               
           END DO k1k2_channels
        END DO L2e


        
     END DO theta_2
  ENDDO theta_1

!  PRINT*, A2e(1,1), ABS(A2e(1,1))**2

  !store W2e(theta_1, theta_2) 
  
  WRITE( A2e_outfile, "(a)") "tout/pad2e.out"
  OPEN( nout, file = A2e_outfile, status = 'replace')
  
  
   store_theta_1: DO ith1 = 1, SIZE(vtheta_1)     ! here it is assumed that the theta_1 is fixed. (size(vtheta_1) = 1)
      store_theta_2: DO ith2 = 1, SIZE(vtheta_2)        
         WRITE(nout,*)  vtheta_2(ith2), ABS(A2e(ith1,ith2)**2 )
      ENDDO store_theta_2
   ENDDO store_theta_1
   
   CLOSE(nout)

   
!  WRITE(*,*) "#                  0 0; 0 0; 0 0 = ", cleb(2*0, 0, 2*0, 0, 2*2, 0), c3j(2*0, 0, 2*0, 0, 2*2, 0)
!  WRITE(*,*) "#                  1 0; 1 0; 0 0 = ", cleb(2*1, 0, 2*1, 0, 2*2, 0), c3j(2*1, 0, 2*1, 0, 2*2, 0) 
!  WRITE(*,*) "#                  2 0; 2 0; 0 0 = ", cleb(2*2, 0, 2*2, 0, 2*2, 0), c3j(2*2, 0, 2*2, 0, 2*2, 0)
!  WRITE(*,*) "#                  3 0; 3 0; 0 0 = ", cleb(2*3, 0, 2*3, 0, 2*2, 0), c3j(2*3, 0, 2*3, 0, 2*2, 0)
!  WRITE(*,*) "#                  4 0; 4 0; 0 0 = ", cleb(2*4, 0, 2*4, 0, 2*2, 0), c3j(2*4, 0, 2*4, 0, 2*2, 0)



END PROGRAM pad2e
