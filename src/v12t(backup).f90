!---22/03/2021---
! Program calculates the 2-e angular Distribution
! for specific energy E, and specific energy sharing e1,e2 between ejected electrons
! run as ./bin/helium_ad2e_Ee1e2  -fieldcycle
!
! fieldcycle refers to the time of evaluation of the distribution e.g the 12th cycle
! of a 12 cycle field is the end of the pulse
!


  ! S( Omega_1, Omega_2)
  !
  !
  ! P(Omega_1,Omega_2) = S_{n,L} S_{l1,l2} C_{n,L}(t) * V_{n1l1;n2l2} * A(n1l1;n2l2) A_{12}{ Y_{L;l1l2}(Omega_1,Omega_2) }
  !
  !
  ! A2e_{12}{ Y_{L;l1l2}(Omega_1,Omega_2) } =
  ! S_{m1 m2}{ C_{L,l1,l2;m1,m2,-M_L} ( Y^*_{l1m1}(Omega_1) * Y^*_{l2m2}(Omega_2) + (-)^Ph * Y^*_{l1m1}(Omega_2)*Y^*_{l2m2}(Omega_2) ) }
  !
  ! Ph = l1 + l2 + L
  !
  ! A(n1l1;n2l2) = (i)^{-(l1 + l2)} e^{ i(delta_{n1l1} + delta_{n2l2} }
  !
  !
  ! the below code should give dP/(de1de2dO_k1 dO_k2) = k1 * k2 *|<k1 k2|psi(t)>|^2
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
  USE w2e, ONLY: read_cfg_file, diagonalize

  IMPLICIT NONE
  !
  REAL(DPK)                                      :: dummie1
  INTEGER                                        :: ll1,ll2, nn1, nn2
  INTEGER                                        :: ne_1e, nl_1e
  INTEGER                                        :: l1e_max
  !
  REAL(dpk), DIMENSION(:,:), POINTER             :: en1e            !nl,ns
  REAL(dpk), ALLOCATABLE, DIMENSION(:,:)         :: e1e             !nl,ns
  REAL(dpk), ALLOCATABLE, DIMENSION(:,:)         :: rho_e1e         !nl,ns
  INTEGER,   ALLOCATABLE, DIMENSION(:)           :: ndi_l           ! pop with  E(ndi_l,l) = E++
  INTEGER,   ALLOCATABLE, DIMENSION(:)           :: nsi_l           ! pop with  E(ndi_l,l) = E+
  INTEGER                                        :: n_l2e           ! nof CI states of symmetry l2e


  INTEGER                                        :: l, ic, ie, iep, icp

  !Wavefunction variables
  TYPE(symmetry_ls2e), ALLOCATABLE, DIMENSION(:) :: w2e
  COMPLEX(DPK), DIMENSION(:,:),   ALLOCATABLE    :: A2e              !bipolar spherical harmonic


  !! cfgs matching (L, n1l1;n2l2) given (l_int, e1_int,l1_int;e2_int,l2_int)
  INTEGER,        DIMENSION(:),  ALLOCATABLE     :: icf, nc1, lc1, nc2, lc2
  COMPLEX(dpk),   DIMENSION(:),  ALLOCATABLE     :: a12
  COMPLEX(DPK)                                   :: ct1, ct2, ph12, ct_nu_t
  REAL(DPK)                                      :: nu1, nu2,
  !  COMPLEX(DPK), DIMENSION(:), ALLOCATABLE        :: cv_t               ! cv_t = S_n c_{nL}(t) * V_{nL;n1l1,n2l2}
  !
  !command line arguments
  REAL(dpk)                                      :: e1_ic, e2_ic, de, v12t

  !input variables
  CHARACTER(len=100)                             :: v12_outfile
  CHARACTER(len=100)                             :: partial_l1, partial_l2, partial_l, num_cycles
  INTEGER                                        :: l1_int, l2_int, l_int, num_cycles_int
  INTEGER                                        :: nv2eb

  !For v12 matrix elements input
  INTEGER                                :: ncs, nhx, ir, ic
  INTEGER,   DIMENSION(:),   POINTER     :: idcs                 !ncs
  INTEGER,   DIMENSION(:),   POINTER     :: ll, nllmin, nllmax   !ncs
  INTEGER,   DIMENSION(:),   POINTER     :: nhf, lhf             !ncs
  INTEGER                                :: lo, inpl, ls
  REAL(dpk), ALLOCATABLE,DIMENSION(:,:)  :: v





  nv2eb = 3

  !get cmd arguments

  CALL getarg(1,  partial_l)
  CALL getarg(2, num_cycles)
  !
  READ(partial_l(1:1),  "(i1)")  l_int
  READ(num_cycles(1:2), "(i2)")  num_cycles_int
  !

  l1_int = 0
  l2_int = 0
  !  l  = l_int

  !WRITE(*,*) "#            (L; l1, l2) = ", l, l1_int, l2_int
  !WRITE(*,*) "# Field cycle  number: c = ",  num_cycles_int


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



  ALLOCATE(rho_e1e(0:nl_1e-1, 1:ne_1e) )  ! e1e(l1_max, n1_max)

  energy_density_1e:DO l = 0, nl_1e
     rho_e1e(l,1) = 1.0_dpk/ABS(e1e(l,2)-e1e(l,1))
     DO ie = 2, ne_1e-1
        rho_e1e(l,ie) = 2.0_dpk/ABS(e1e(l,ie-1)-e1e(l,ie+1))
     ENDDO
     rho_e1e(l,ne_1e) = 1.0_dpk/ABS(e1e(l,ne_1e-1)-e1e(l,ne_1e))

  ENDDO energy_density_1e

  DO ie = 1, ne_1e-11
     WRITE(20,*) e1e(1,ie), ABS(e1e(1,ie-1)-e1e(1,ie+1)), rho_e1e(1,ie)
  ENDDO


  DEALLOCATE(en1e)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Calculate SI, DI thresholds

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



  ! The electron's energies should be fixed as well. In the below  e1 and E are fixed)

  !
  ! find the configuration channels consistent with the | k_1,k_2 > asymptotic wf2e
  !

     de = 0.018   !0.5_dpk * ABS(e1e(ll1,nn1+1)-e1e(ll1,nn1-1))

     ALLOCATE(icf(0))
     ALLOCATE(a12(0))
     ALLOCATE(nc1(0))
     ALLOCATE(lc1(0))
     ALLOCATE(nc2(0))
     ALLOCATE(lc2(0))


     nn1 = 0
     nn2 = 0
     ll1 = 0
     ll2 = 0
     PRINT*, '#                                                    de = ', ABS(de)

     select_cfg_in_L: DO ic = 1, w2e(l_int)%ncf                   ! Sum over all cfs (for L)

        !short-cut definitions

        nn1 = w2e(l_int)%n1(ic)        ! n1
        ll1 = w2e(l_int)%l1(ic)        ! l1
        !
        nn2 = w2e(l_int)%n2(ic)        ! n2
        ll2 = w2e(l_int)%l2(ic)        ! l2


        e1_ic = e1e(ll1,nn1)
        e2_ic = e1e(ll1,nn2)





        ! exclude any (n1l1;n2l2) that does not match the selection criteria


        IF(e1_ic < 0) CYCLE
        IF(e2_ic < 0) CYCLE


        del_kr = 0.0_dpk
        IF( (ABS(e1_ic - e1_int) < ABS(de)).AND.( ABS(e2_ic-e2_int) < ABS(de)) ) THEN

           !IF( (ll1 == l1_int).AND.(ll2 == l2_int) )  THEN
           !IF(ll1==ll2)  THEN

              nc1  = [nc1, nn1]
              nc2  = [nc2, nn2]
              lc1  = [lc1, ll1]
              lc2  = [lc2, ll2]
              icf  = [icf, ic]

              PRINT*, '#                           ic    (nn1, ll1; nn2 ll2) = ', ic, " = ", nn1, ll1, nn2, ll2
              PRINT*, '#              (e2_ic - e1_int),      e1_ic,  e1_int  = ', ABS(e1_ic - e1_int), e1_ic, e1_int
              PRINT*, '#              (e1_ic - e2_int),       e2_ic,  e2_int = ', ABS(e2_ic - e2_int), e2_ic, e2_int

              !short-cut definitions
              k1   = k_l(ll1, nn1)
              k2   = k_l(ll2, nn2)
              !
              rho1 = rho_e1e(ll1,nn1)
              rho2 = rho_e1e(ll2,nn2)

!              del_kr = (k1 + k2) * rmax  + (2.0_dpk/k1) * LOG( 2.0_dpk * k1 * rmax) + (2.0_dpk/k2) * LOG(2.0_dpk * k2 * rmax )
!              ph12 = (-1)**(nn1 +  nn2)  * SQRT(rho1)*SQRT(rho2)*( COS(del_kr) + zi * SIN(del_kr))

              del_kr =  -(ll1 + ll2) * m_pi*0.5_dpk + sigma_kl(ll1, 2.0_dpk, k1) +  sigma_kl(ll2, 2.0_dpk, k2)
              ph12   = sqrt(rho1) * sqrt(rho2) * (COS(del_kr) + zi * SIN(del_kr))

              !
              ! ph12 = (-1)**(nc1(ic) + nc2(ic)) * cdexp( - zi * ( d_lk(lc1(ic),nn1(ic) ) + d_lk(ll2(ic),nn2(ic)) )
              !                   * cdexp(-zi * ( (k1 + k2) * rmax  + (2/k1) * LOG( 2 * k1 * rmax) + (2/k2)*LOG(2*k2*rmax) ) )
              ! a12  = [a12, ph12]
              !

              IF(ll1 == ll2) THEN
               a12  = [a12, 0.5*ph12]
              ELSE
                 a12  = [a12, ph12]
              ENDIF

           !ENDIF

        ELSE IF( (ABS(e2_ic - e1_int) < ABS(de)).AND.( ABS(e1_ic-e2_int) < ABS(de)) ) THEN
           !IF( (ll1 == l1_int).AND.(ll2 == l2_int) )  THEN
           !IF(ll1==ll2)  THEN

              nc1  = [nc1, nn1]
              nc2  = [nc2, nn2]
              lc1  = [lc1, ll1]
              lc2  = [lc2, ll2]
              icf  = [icf, ic]

              k1   = k_l(ll1, nn1)
              k2   = k_l(ll2, nn2)
              rho1 = rho_e1e(ll1,nn1)
              rho2 = rho_e1e(ll2,nn2)

              !box phase shift

!              del_kr = (k1 + k2) * rmax  + (2.0_dpk/k1) * LOG( 2.0_dpk * k1 * rmax) + (2.0_dpk/k2) * LOG(2.0_dpk*k2*rmax)
!              ph12 = (-1)**(nn1 + nn2)  * SQRT(rho1) * SQRT(rho2) * ( COS(del_kr) + zi * SIN(del_kr) )

              del_kr =  -(ll1 + ll2) * m_pi*0.5_dpk  + sigma_kl(ll1, 2.0_dpk, k1) + sigma_kl(ll2, 2.0_dpk, k2)
              ph12 = SQRT(rho1) * SQRT(rho2) * ( COS(del_kr) + zi * SIN(del_kr) )

              !analytical
              !              IF(ll1.EQ.ll2) THEN

              !ph12 = (-1)**(nc1(ic) + nc2(ic)) * cdexp( - zi * ( d_lk(lc1(ic),nn1(ic) ) + d_lk(ll2(ic),nn2(ic)) )
              !ph12 = (-1)**((nn1 + nn2) )  &
              !     * cdexp(-zi * ( (k1 + k2) * rmax  + (2.0_dpk/k1) * LOG( 2 * k1 * rmax) + (2.0_dpk/k2)*LOG(2.0_dpk*k2*rmax) ) )
              !              a12  = [a12, ph12]

              IF(ll1 == ll2) THEN
                 a12  = [a12, 0.5*ph12]
              ELSE

                 a12  = [a12, ph12]
              ENDIF

              PRINT*, '#                          icx    (nn1, ll1; nn2 ll2) = ', ic, " = ", nn1, ll1, nn2, ll2
              PRINT*, '#              (e2_ic - e1_int),      e1_ic,  e1_int  = ', ABS(e2_ic - e1_int), e1_ic, e1_int
              PRINT*, '#              (e1_ic - e2_int),       e2_ic,  e2_int = ', ABS(e1_ic - e2_int), e2_ic, e2_int

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



!read electrostatic matrix elements v12!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    CALL read_cfg_file(l_int, ls, nhf,lhf,ll, nllmin, nllmax, idcs, ncs, nhx)
    CALL v12file(nv2eb, l_int)

    ALLOCATE( v(nhx,nhx) )

    read_v12_upper: DO  ir = 1, SIZE(v,dim=1)

       READ(nv2eb) ( v(ir,ic), ic = ir, SIZE(v,dim=2))

       fill_lower_part:DO  ic = ir, SIZE(v,dim=2)   !fill matrix since symmetric

          v(ic,ir) = v(ir,ic)

       ENDDO fill_lower_part
    ENDDO read_v12_upper

    CLOSE(nv2eb)
!Close electrostatic matrix elements file!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



    v12t = 0.0_dpk  !at this point, depends on L

    N2e:DO ie = 1, w2e(l_int)%net   !DI 2e-states included in the psi(\r1,\r2,t) for 'L' symmetry

      ct1 = CONJG(w2e(l_int)%ct(ie))

      N2e_prime: Do iep = 1, w2e(l_int)%net  !same as previosu summation for primed variables

         ct2 = ct1 * w2e(l_int)%ct(iep)

         l1l2:DO ic = 1, w2e(l_int)%ncf   !channels {L_int; n1_int, l1_int,n2_int, l2_int}

            nu1 = w2e(l_int)%cv(ie,ic)

            l1l2_prime:DO icp = 1, w2e(l_int)%ncf   !channels {L_int; n'1_int, l'1_int,n'2_int, l'2_int}

               nu2 = nu1 * w2e(l_int)%cv(ie,icp)

               ct_nu_t = ct2 * nu2

               v12t = v12t + (ct_nu_t * v(ic,icp))

            END DO l1l2_prime
         END DO l1l2
       END DO N2e_prime
    END DO N2e



  WRITE( v12_outfile, "(a)") "tout/v12_total.out"
  OPEN( nout, file = v12_outfile, status = 'replace')



         WRITE(nout,*)  l_int, v12t

   CLOSE(nout)


END PROGRAM pad2e
