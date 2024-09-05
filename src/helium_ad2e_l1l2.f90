!---22/03/2021---
!Program calculates the 2-e angular Distribution
!l1, l2 partial distribution, summed over all L, E, e1, e2
!run as ./bin/helium_ad2e_l1l2  -l1  -l2  -fieldcycle

!fieldcycle refers to the time of evaluation of the distribution e.g the 12th cycle
!of a 12 cycle field is the end of the pulse

program ad2e_test

    use pad_utils
    use parameter_tdse_fxd
    use bs_frb_2e,      only: read_target_energies
    use io
    use param,          only: np  !Number of wavefunction grid points
    use precision
    use atom_2e                 !Provides routines for reading CI/TDSE coefficients
    use anglib                  !module for clebsch-gordon Coefficients
    use spherical_harmonics     !module for spherical harmonics
    use core_phase_shift

    implicit none

    real(DPK) :: pi
    real(DPK) :: dummie1, dummie2, dummie4 ,dummie5
    CHARACTER(len=20) :: dummie3

    integer   :: ll1,ll2, nn1, nn2, nof_l_ncf
    INTEGER   :: ne_1e, nl_1e
    INTEGER   :: l1e_max, dummie6

    real(dpk), dimension(:,:), pointer :: en1e        !nl,ns
    REAL(dpk), ALLOCATABLE, DIMENSION(:,:)         :: e1e                 !nl,ns
    integer,   allocatable, dimension(:)        :: ndi_l           ! pop with  E(ndi_l,l) = E++
    integer,   allocatable, dimension(:)        :: nsi_l           ! pop with  E(ndi_l,l) = E+
    integer                                     :: n_l2e           ! nof CI states of symmetry l2e
    integer                                     :: cycle  !cycle number (time of evaluation of distribution)

!Redundant variables now after reformulating loops. Fix this.
    integer                 :: i, ijmax                !Index for energies
    integer                 :: l, l1max                !Index for angular momenta
    integer                 :: ic, ie, ie_max    !,ie_p        !Index for reformulated summations in terms of configuration number
    integer                 :: nof_points, nof_points2, nof_points_angular
    real(dpk)               :: angle_step
    integer :: mod_dummy

    !Wavefunction variables
    type(symmetry_ls2e), allocatable, dimension(:) :: w2e
    integer                 :: ir!, ir_skip            Wavefunction point index, i.e piont 1, 2, etc
    real(DPK), dimension(:), allocatable :: r          !Wavfefunction grid values
    real(DPK), dimension(:,:,:), allocatable:: p       !Wavefunction values, P_nl(r)
    complex(DPK), dimension(:), allocatable:: work_A !bipolar spherical harmonic
    complex(DPK), dimension(:), allocatable:: A !bipolar spherical harmonic

    real(DPK), dimension(:,:), allocatable:: y !bipolar spherical harmonic
    real(DPK) :: phi1, phi2, theta1r, theta2r
    integer :: m1, m2, theta1, theta2
    real(DPK) :: pop_ic
    complex(DPK):: pop_t_ie


    character(len=32) :: wf1efilename, phasefilename, ad2efilename          !Wavefunction data file
    logical :: file_exists

    !command line arguments
    character(len=100):: partial_l1, partial_l2, num_cycles, rd2efilename
    integer:: l1_int, l2_int, num_cycles_int, l1_select, l2_select, k_val

    !For phase shifts
    real(DPK), dimension(:,:), allocatable :: phase !delta_kl array for phase shift values
    real(DPK), dimension(:,:), allocatable :: k_value !delta_kl array for phase shift values
    integer :: i_n1, i_n2 !index for summing over one-electron energies
    integer :: ki, R1, R2, l2, j, av
    integer :: reduced_ijmax, k !only energies up to roughly n=40 have valid calculated radial functions. Use this instead.
    real(dpk) :: qk, zk, k1, k2
    pi = 4.D0*DATAN(1.D0)



!get cmd arguments
!    call getarg(1, L_total)
    call getarg(1, partial_l1)
    call getarg(2, partial_l2)
    call getarg(3, num_cycles)
    read(partial_l1(1:1), "(i1)") l1_int
    read(partial_l2(1:1), "(i1)") l2_int
    read(num_cycles(1:2), "(i2)") num_cycles_int
    write(*,*) num_cycles_int, "Field cycle number:"
    write(*,*) l1_int, "l1"
    write(*,*) l2_int, "l2"


   !read(L_total(1:1), "(i1)") L_int


!----------------------------Get number of states 1e from inp/h1e.inp -------------------------
    open(unit = 8, file = "inp/h1e.inp", status = 'old')
        !Dummie file read
        read(8, *)       dummie1, dummie2
        read(8, *)       dummie3, dummie4, dummie5
        !read number of B-splines
        read(8, *)       ijmax, dummie6
        write(*,*) "#Number of b-splines = ", ijmax
        ijmax = ijmax - 2
        write(*,*) "#Number of eigenstates = ", ijmax
    close(8)






      !--------------------------R￼ead TDSE coeffs and CI coeffs and configuration data-------------------------

      call input_tdse_fxd              ! tinp/tdse_bs_fxd_2e.inp
      call output_tdse_fxd             ! tout/tdse_bs_fxd_2e.out
      call read_target_energies(en1e)  ! read 1e energies (in Ryd)

      write(*,*) "Calculating field cycle", num_cycles_int


      call read_w2e_ct(w2e,lmax,num_cycles_int)
      write(*,*) "Calculating field cycle", num_cycles_int
      do l = 0, lmax
         call read_w2e_ci(w2e(l),l)     !reads coeffs Phi_j = \sum_i v_{ij} Phi^{(0)}_{i}
      end do





      !!!!!!!!1-electron energy matrix initialisations

      en1e = en1e * 0.5_dpk              ! convert energies in a.u.
      nl_1e = SIZE(en1e,dim=1)  ! e1e(l1_max, n1_max)
      ne_1e = SIZE(en1e,dim=2)  ! e1e(l1_max, n1_max)

      ! for conformity reasons we rewrite
      ! en1e(1:nl_1e,1:ne_1e) --> e1e(0:nl_1e-1,1:ne_1e) -->

      !First find the lread_target_energiesargest angular momentum included in the cfg channels.
      l1e_max = 0
      DO l = 0, lmax
         IF(SIZE(w2e(l)%l12,dim=1).GT.l1e_max) l1e_max = SIZE(w2e(l)%l12,dim=1)
      ENDDO
      l1e_max = l1e_max - 1   ! since l1e_max inside the loop returns the size of l12

      PRINT*,"&             l1e_max = ", l1e_max
      IF((nl_1e-1).LT.l1e_max) THEN
         PRINT*,"something wrong here:"
         PRINT*," max l1e calculated              ne_1e = ", nl_1e-1
         PRINT*," max l1e included in cfg files l1e_max = ", l1e_max
         STOP
      ENDIF
      ALLOCATE( e1e(0:l1e_max,1:ne_1e))

      DO l = 0, l1e_max
         DO ie = 1, ne_1e
            e1e(l,ie) = en1e(l+1,ie)
         ENDDO
      ENDDO
      DEALLOCATE(en1e)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!Calculate ionisatation thresholds

ALLOCATE( ndi_l(0:lmax) )
ALLOCATE( nsi_l(0:lmax) )


     ndi_l =  0
     nsi_l  =  0
     find_si_di_thresholds:DO l = 0, lmax

        n_l2e = w2e(l)%net

        si:DO  ie = 1, n_l2e     ! find SI threshold energy  for symmetry L
           nsi_l(l) = ie
           !        IF(  w2e(l)%e2e(ie) > 0.0_dpk ) EXIT
           IF(  w2e(l)%e2e(ie) > en_ion_1 ) EXIT
        ENDDO si


     di:DO  ie = 1, n_l2e     ! find DI threshold energy  for symmetry L
        ndi_l(l) = ie

        ! since 2-electron systemd by definition en_ion_2 = 0.0_dpk

        IF( ( w2e(l)%e2e(ie) ) > 0.0_dpk ) EXIT
     ENDDO di
     WRITE(*,*) "tdse_sdi_pop:: si threshold              en_ion_1 = ", en_ion_1
     WRITE(*,*) "tdse_sdi_pop:: si threshold  E(", nsi_l(l), l, ") = ", w2e(l)%e2e(nsi_l(l))
     WRITE(*,*) "tdse_sdi_pop:: di threshold  E(", ndi_l(l), l, ") = ", w2e(l)%e2e(ndi_l(l))
     WRITE(*,*) "tdse_sdi_pop:: max energy    E(", n_l2e, l,    ") = ", w2e(l)%e2e(n_l2e)

     !     IF(nsi_l(l).EQ.1) THEN
     !        WRITE(*,*) "tdse_sdi_pop:: ie for SI threshold can't be ", nsi_l(l)
     !        STOP
     !     ENDIF

  ENDDO find_si_di_thresholds



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!calculate phase shifts

      reduced_ijmax = 60
    !  av = 100

    !  allocate(phase(0:3, 1:reduced_ijmax))  !phase shifts stored in this array
      allocate(k_value(0:3, 1:reduced_ijmax))  !array for storing K values corresponding to 1e energies

      !phase = 0.0_dpk
      k_value = 0.0_dpk

        loop_l: do l = 0, 3
          loop_k: do ki = 1, reduced_ijmax

            !calculate K values from 1e energies. If bound state energy, assign -1 as placeholder
            if (e1e(l,ki) .lt. 0) then
              k_value(l,ki) = -1.0
        !      phase(l,ki) = -1.0
            else
              k_value(l,ki) = sqrt(2*e1e(l,ki))
          !    phase(l,ki) = k_value(l,ki)*60.0 - l*(pi/2) + (2.0/k_value(l,ki))*log(2.0*k_value(l,ki)*60.0)+&
          !    sigma_kl(l,2.0_dpk,k_value(l,ki))
          !    write(*,*) phase(l,ki)
            end if


            !(p(R2, ki, (l+1))*bessel_jn(l,((k_value(l,ki))*(r(R1)))) - p(R1, ki, (l+1))*&
            !bessel_jn(l,((k_value(l,ki))*(r(R2)))))/ &
            !(p(R1, ki, (l+1))*bessel_yn(l,((k_value(l,ki))*(r(R2)))) - p(R2, ki, (l+1))*bessel_yn(l,((k_value(l,ki))*(r(R1)))))

              !  phase(l,ki,j) = atan(phase(l,ki,j))
              !  phase(l,ki,(av+1)) = phase(l,ki,(av+1)) + phase(l,ki,j)

            !phase(l,ki,(av+1)) = phase(l,ki,(av+1))/av

            !if (phase(l,ki,(av+1)) .lt. 0.0) then
            !  phase(l,ki,(av+1)) = phase(l,ki,(av+1)) + 3.14159
            !end if

          !  write(*,*) e1e(l,ki), k_value(l,ki), phase(l,ki,(av+1)) !write energies, k's and phases

          end do loop_k

        end do loop_l

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!save phases

      !  loop_l2: do l = 0, 3

        !  write (phasefilename, '(a,I1,a)') 'phaseshift-',l,'.out'
          !open(19, file = phasefilename, status = 'replace')

          !loop_k2: do ki = 1, reduced_ijmax

            !if ( k_value(l,ki) .gt. 0) then
              !write(*,*) l, k_value(l,ki), phase(l,ki,(av+1))!¬, (phase(l,ki,(av+1))-((l*pi)/2)+(k_value(l,ki)*r(nof_points)))/pi
              !write(19,'(3E20.10)')  k_value(l,ki), phase(l,ki,(av+1))
          !  end if

          !end do loop_k2

        !  close(19)

        !end do loop_l2


        !write(*,*) "finished phases"

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!calcualte angular distribution


        nof_points_angular = 400
        write(*,*) "number of points=", nof_points_angular
        allocate( work_A(1:nof_points_angular))
        allocate( A(1:nof_points_angular))

        allocate( y(1:nof_points_angular, 1:nof_points_angular))

       !allocate( work_A(1:8, 1:8, 1:4, 1:nof_points_angular, 1:nof_points_angular))

        y = 0.0_dpk
        A = 0.0_dpk

        phi1 = 0
        phi2 = 0
        m1 = 0
        m2 = 0
        l1_select = l1_int
        l2_select = l2_int
        write(*,*) "working partial wave:", l1_select, l2_select




        write(rd2efilename,"(a,i1,a,i1,a)") "ad2e/ad2e_l1l2_",l1_select, "_", l2_select, ".out"
          open(19, file = rd2efilename, status = 'replace')

        loop_n1: do i_n1 = 1, 60
          loop_n2: do i_n2 = i_n1, 60

            write(*,*) "working", i_n1, i_n2
            work_A = 0.0_dpk

              loop_total_L: do l = 0, 3

                write(*,*) "Working total angular momentum:", l

                nof_l_ncf = w2e(l)%ncf
                ie_max = w2e(l)%net

                  loop_total_energy: do ie = ndi_l(l), ie_max           !write(*,*) ie

                    pop_t_ie = w2e(l)%ct(ie)                            !TDSE coefficients

                      loop_l1l2: do ic = 1, nof_l_ncf         !sum from configuration 1 to max number of configurations, for the total  L

                        nn1 = w2e(l)%n1(ic) !nn1 is energy index. also indexes k values
                        nn2 = w2e(l)%n2(ic)
                        if (((nn1 .eq. i_n1) .and. (nn2 .eq. i_n2)) .or. ((nn2 .eq. i_n1) .and. (nn1 .eq. i_n2))) then !select only specific k1 and k2

                        ll1 = w2e(l)%l1(ic)
                        ll2 = w2e(l)%l2(ic)

                        if((k_value(ll1,nn1) .lt. 0) .or. (k_value(ll2,nn2) .lt. 0)) THEN
                          !write(*,*) k_value(ll1,nn1), k_value(ll2,nn2)
                          cycle loop_l1l2
                        end if

                        if((ll1 .eq. l1_select) .and. (ll2 .eq. l2_select)) then

                        k1 = k_value(ll1,nn1)
                        k2 = k_value(ll2,nn2)

                        pop_ic = w2e(l)%cv(ie,ic)

                        theta1r = 0
                        theta2r = 0
                        angle_step = 2*pi/nof_points_angular

                        if (((nn1 .eq. nn2) .and. (ll1 .eq. ll2))) then
                          loop_angle: do theta2 = 1, nof_points_angular+1
                            work_A(theta2) =  work_A(theta2) + &
                            sphharm(ll1, 0, 0.0_dpk, 0.0_dpk)*sphharm(ll2, 0, theta2r, 0.0_dpk)*cleb(2*ll1, 0, 2*ll2, 0, 2*l, 0)*& !spherical harmonics* clebsch coefficients
                            pop_t_ie*pop_ic*cdexp(-(0.0,1.0)*((k1+k2)*60 + (2/k1)*log(2*k1*60) + (2/k2)*log(2*k2*60)))*& !logarithmiic phase z/k ln(2kr)
                            (-1)**((nn1-1) + (nn2-1)) !phase term from number of nodes number of nodes for (-1)^(m1+m2)
                            theta2r = theta2r + angle_step
                          end do loop_angle
                        else
                          loop_angle_exchange: do theta2 = 1, nof_points_angular+1
                            work_A(theta2) =  work_A(theta2) + &
                            sphharm(ll1, 0, 0.0_dpk, 0.0_dpk)*sphharm(ll2, 0, theta2r, 0.0_dpk)*cleb(2*ll1, 0, 2*ll2, 0, 2*l, 0)*&
                            pop_t_ie*2*pop_ic*cdexp(-(0.0,1.0)*((k1+k2)*60 + (2/k1)*log(2*k1*60) + (2/k2)*log(2*k2*60)))*& !logarithmiic phase z/k ln(2kr)
                            (-1)**((nn1-1) + (nn2-1)) !phase term from number of nodes number of nodes for (-1)^(m1+m2)
                            theta2r = theta2r + angle_step
                          end do loop_angle_exchange
                        end if

                      end if

                    end if

                  end do loop_l1l2

              !write(*,*)

              end do loop_total_energy
            end do loop_total_L

            if (i_n1 .eq. i_n2) then
              A = A + abs(work_A)**2
            else
              A = A + 2*abs(work_A)**2
            end if

          end do loop_n2
        end do loop_n1




        write(*,*) "fail here 1"

        do theta1 = 1, nof_points_angular

            write(*,*)  ((theta1-1)*angle_step)*(180/pi), abs(A(theta1))
            write(19,'(3E20.10)')  ((theta1-1)*angle_step)*(180/pi), abs(A(theta1))

        end do



        write(*,*) "start close"
          close(19)
        write(*,*) "finished close"



deallocate(A)
deallocate(work_A)
deallocate(y)
!deallocate(phase)
!deallocate(k_value)
end program ad2e_test
