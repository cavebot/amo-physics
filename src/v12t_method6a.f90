PROGRAM enlnl_program

  USE PRECISION
  USE io,             ONLY:nout, ninp
  !USE units,          ONLY: m_pi, zi
  USE param,          ONLY: rmax
  USE bs_frb_2e,      ONLY: read_target_energies
  USE atom_2e                                      !Provides routines for reading CI/TDSE coefficients
  USE parameter_tdse_fxd
  !USE w2e, ONLY: read_cfg_file, diagonalize

  IMPLICIT NONE
  !
  REAL(DPK)                                      :: dummie1
  INTEGER                                        :: ll1,ll2, nn1, nn2
  INTEGER                                        :: ne_1e, nl_1e
  INTEGER                                        :: l1e_max, delta
  !
  REAL(dpk), DIMENSION(:,:), POINTER             :: en1e            !nl,ns
  REAL(dpk), ALLOCATABLE, DIMENSION(:,:)         :: e1e             !nl,ns
  REAL(dpk), ALLOCATABLE, DIMENSION(:,:)         :: rho_e1e         !nl,ns
  INTEGER,   ALLOCATABLE, DIMENSION(:)           :: ndi_l           ! pop with  E(ndi_l,l) = E++
  INTEGER,   ALLOCATABLE, DIMENSION(:)           :: nsi_l           ! pop with  E(ndi_l,l) = E+
  INTEGER                                        :: n_l2e           ! nof CI states of symmetry l2e


  INTEGER                                        :: l, ic, ie, iep, icp, ie_max, ic_max

  !Wavefunction variables
  TYPE(symmetry_ls2e), ALLOCATABLE, DIMENSION(:) :: w2e

  !! cfgs matching (L, n1l1;n2l2) given (l_int, e1_int,l1_int;e2_int,l2_int)
  INTEGER,        DIMENSION(:),  ALLOCATABLE     :: icf, nc1, lc1, nc2, lc2
  COMPLEX(DPK)                                   :: ct1, ct2, ct3, ct_nu_t
  REAL(DPK)                                      :: nu1, nu2, nu3, nu4, nu5, nu, ct

  !input variables
  CHARACTER(len=100)                             :: partial_l1, partial_l2, partial_l, num_cycles
  INTEGER                                        :: l1_int, l2_int, l_int, num_cycles_int, starting_state

  !For matrix elements input

  REAL(dpk), ALLOCATABLE,DIMENSION(:,:)  :: vt, enlnl, dnlnl
  REAL(dpk)                              :: v12t, ht, dt, h_0t, e1, e2, e_tilde, eevv, EE, CC, fl1l2l, eevv1, eevv2, eevv3
  CHARACTER(len=100)                     :: v12_outfile, enlnl_outfile, Hout_outfile




  !get cmd arguments
  CALL getarg(1,  partial_l)
  !CALL getarg(2,  partial_l1)
  !CALL getarg(3,  partial_l2)
  CALL getarg(2, num_cycles)
  !
  READ(partial_l(1:1),  "(i1)")  l_int
  !READ(partial_l1(1:1),  "(i1)")  l1_int
  !READ(partial_l2(1:1),  "(i1)")  l2_int
  READ(num_cycles(1:2), "(i2)")  num_cycles_int
  !
  write(*,*) l_int, num_cycles_int
!  write(*,*) l1_int, l2_int
  !l1_int = 0
  !l2_int = 0
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





  write(*,*) "working L = ", l_int
!  write(*,*) "working l1 = ", l1_int
!  write(*,*) "working l2 = ", l2_int
  write(*,*) "working cycle = ", num_cycles_int

  ie_max = w2e(l_int)%net
  ic_max = w2e(l_int)%ncf
  write(*,*) "ie_max = ", ie_max
  write(*,*) "ic_max = ", ic_max
  !write(*,*) "Running above continuum energy limit N =", ndi_l(l_int)

  write(*,*)
  write(*,*) "Method 6: Correct sign + all states and configurations (including Ground State) + first term. Partial in L"


  fl1l2l = 0
  CC = 0
  eevv = 0

  ht = 0 !H(t) initialize

  if(l_int .eq. 0) then !exclude ground state from summation
    starting_state = 1
  else
    starting_state = 1
  end if

  write(*,*) "starting state = ", starting_state

  DO ie = starting_state , ie_max   !SUM OVER N
    ct = abs(w2e(l_int)%ct(ie))**2
    ht = ht + (ct * w2e(l_int)%e2e(ie)*0.5)
  END DO


  write(*,*) "Hamiltoniain continuum total energy = ",  ht


       loop_configurations: do ic = 1, ic_max!sum from configuration 1 to max number of configurations, for the total  L

         !write(*,*) ic

         ll1 = w2e(l_int)%l1(ic)
         ll2 = w2e(l_int)%l2(ic)

         !if(((ll1) .ne. (l1_int)) .or. ((ll2) .ne. (l2_int))) then
           !write(*,*) ll1, ll2
        !   cycle loop_configurations
         !end if

         nn1 = w2e(l_int)%n1(ic)
         nn2 = w2e(l_int)%n2(ic)


         if((ll1 .eq. ll2) .and. (nn1 .eq. nn2)) then
           delta = 1  !equivalent electron case
         else
           delta = 0  !non-equivalent electron case
         end if

         e1  = e1e(ll1, nn1)
         e2  = e1e(ll2, nn2)

         !if((e1 < 0) .or. (e2 < 0)) then
           !cycle loop_configurations
         !end if

         e_tilde = (e1 + e2) / (1+delta)

         eevv = 0
         eevv1 = 0



         N2e:DO ie = starting_state, (ndi_l(l_int))   !SUM OVER N

           ct1 = w2e(l_int)%ct(ie)
           CC = abs(ct1)**2

           nu1 = w2e(l_int)%cv(ie,ic) !Configuration interaction coefficients
           nu = nu1**2
           !nu2 = w2e(l_int)%cv(iep,ic)

           eevv1 =  (nu * CC) !2 resulting from the equation

            eevv2 = 0
            N2e_prime: DO iep = (ie+1), ndi_l(l_int)  !SUM OVER N', upper triangle

              nu2 = w2e(l_int)%cv(ie,ic)
              nu3 = w2e(l_int)%cv(iep,ic)
              ct2 = w2e(l_int)%ct(ie)
              ct3 = w2e(l_int)%ct(iep)

              eevv2 = eevv2 + REALPART((CONJG(ct3)*ct2)) * nu2 * nu3

            END DO N2e_prime

            eevv = eevv + (-2*eevv1 - 4*eevv2) * e_tilde

         END DO N2e

         fl1l2l = fl1l2l + eevv


       end do loop_configurations


       write(*,*) fl1l2l

       write(*,*) "GS ENERGY = ", w2e(0)%e2e(1)*0.5
       write(*,*) "e1+e2 GS= ", e1e(0,1)*2



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
WRITE(Hout_outfile, "(a,i1,a,i1,a,i1,a)") "v12t/method6a/v12t_method6a_", l_int, ".out"
OPEN(3, file = Hout_outfile, access = 'append')

write(3,'(i2,3E20.10)') num_cycles_int, (ht + fl1l2l)
close(3)


END PROGRAM enlnl_program
