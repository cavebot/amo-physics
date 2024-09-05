PROGRAM pad2e

  USE PRECISION
  USE io,             ONLY:nout, ninp
  USE units,          ONLY: m_pi, zi
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
  !! cfgs matching (L, n1l1;n2l2) given (l_int, e1_int,l1_int;e2_int,l2_int)
  INTEGER,        DIMENSION(:),  ALLOCATABLE     :: icf, nc1, lc1, nc2, lc2
  COMPLEX(dpk),   DIMENSION(:),  ALLOCATABLE     :: a12
  COMPLEX(DPK)                                   :: ct1, ct2, ph12, ct_nu_t
  REAL(DPK)                                      :: nu1, nu2
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
  INTEGER                                :: ncs, nhx, ir
  INTEGER,   DIMENSION(:),   POINTER     :: idcs                 !ncs
  INTEGER,   DIMENSION(:),   POINTER     :: ll, nllmin, nllmax   !ncs
  INTEGER,   DIMENSION(:),   POINTER     :: nhf, lhf             !ncs
  INTEGER                                :: lo, inpl, ls
  REAL(dpk), ALLOCATABLE,DIMENSION(:,:)  :: v, v_exclusions




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



  !CALL read_cfg_file(l_int, ls, nhf,lhf,ll, nllmin, nllmax, idcs, ncs, nhx)



!These loops are needed for configuration exclusions
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !ALLOCATE(   n_principle(ncs) )
  !ALLOCATE(  nd(ncs) )
  !ALLOCATE(  is(ncs) )

  !n_principle = nhf + lhf                  !principal q.number
  !nd = nllmax - nllmin + 1

  !ncmx = 0
  !DO k = 1, ncs
  !   is(k) = ncmx + 1
  !   ncmx  = ncmx + nd(k)
  !ENDDO
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!






!!!read r12 matrix elements!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ALLOCATE( v(nhx,nhx) )

  !CALL v12file(nv2eb, l_int)

  !read_v12_upper: DO  ir = 1, SIZE(v,dim=1)

  !   READ(nv2eb) ( v(ir,ic), ic = ir, SIZE(v,dim=2))

  !   fill_lower_part:DO  ic = ir, SIZE(v,dim=2)   !fill matrix since symmetric

  !      v(ic,ir) = v(ir,ic)

  !   ENDDO fill_lower_part
  !ENDDO read_v12_upper

  !CLOSE(nv2eb)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



  ALLOCATE(    v_exclusions(nhx,nhx) ) !assign matrix for reduced matrix?
  !NOT NECESSARY FOR HELIUM CASE APPARENTLY BUT I INCLUDE THE PROCESS ANYWAY
  !JUST IN CASE...


  !These loops are needed for configuration exclusions
  !ALLOCATE(ih(nhx))
  !ih = 0

  !define_ihd:DO  k = 1, ncs
  !   IF(nd(k).EQ.0) CYCLE
  !   ihd = is(k) - 1
  !   DO  id = 1, nd(k)
  !      ihd = ihd + 1
  !      ih(ihd) = 1
  !   ENDDO
  !ENDDO define_ihd
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



  !These loops are needed for configuration exclusions
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !# exclude configurations from r12 matrix
  !k = 0
  !include_cfg:DO  ir = 1, SIZE(v,dim=1)
  !   IF(ih(ir).EQ.0) CYCLE     ! exclude
  !   k = k + 1
  !   kk = 0
  !   DO  ic = 1, SIZE(v,dim=2)
  !      IF(ih(ic).EQ.0) CYCLE   ! exclude
  !      kk = kk + 1
  !      v_exclusions(k, kk) = v(ir, ic)
  !   ENDDO
  !ENDDO include_cfg
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



  !compare pre and post exclusion matrix sizes
  write(*,*) size(v,dim=1)
  write(*,*) size(v_exclusions,dim=1)
  !If these sizes are not equal then configuration exclusions have taken place
  !This will have to be taken into account



!!!!!!!!!!CALCULATE V12(t) USING TDSE AND CI COEFFICIENTS AND V12 MATRIX ELEMENTS!!!!!!!!
  v12t = 0.0_dpk  !at this point, depends on L!!!!!!!!!!!!!!!!!!!!!!!!!!!
  write(*,*) "working L = ", l_int

  N2e:DO ie = 1, w2e(l_int)%net   !SUM OVER N

    write(*,*) ie
    ct1 = CONJG(w2e(l_int)%ct(ie))

    N2e_prime: Do iep = 1, w2e(l_int)%net  !SUM OVER N'

       write(*,*) iep
       ct2 = ct1 * w2e(l_int)%ct(iep)

       l1l2:DO ic = 1, w2e(l_int)%ncf   !SUM OVER n1l1n2l2

          nu1 = w2e(l_int)%cv(ie,ic)

          l1l2_prime:DO icp = 1, w2e(l_int)%ncf   !SUM OVER n1'l2'n2'l2'

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
