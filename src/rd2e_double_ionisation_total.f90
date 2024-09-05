!---25/01/2019---
!Program calculates the 2-e Radial Distribution in terms of
!the 1-e radial wavefunctions, configuaration interaction coefficients,
!and TDSE coefficients according (Reference to be added)

program rd2e

    use parameter_tdse_fxd
    use bs_frb_2e,      only: read_target_energies
    use io
    use param,          only: np  !Number of wavefunction grid points
    use precision
    use atom_2e                 !Provides routines for reading CI/TDSE coefficients

    implicit none


    real(DPK) :: dummie1, dummie2, dummie3, dummie4 ,dummie5
    integer   :: dummie6
    real(DPK) :: dummie7
    integer   :: ll1,ll2, nn1, nn2, nof_l_ncf
    real(DPK) :: e1, e2 !single electron energies
    integer   :: k1, k2
    INTEGER   :: ne_1e, nl_1e
    INTEGER   :: l1e_max
    real(dpk), dimension(:,:), pointer :: en1e        !nl,ns
    REAL(dpk), ALLOCATABLE, DIMENSION(:,:)         :: e1e                 !nl,ns
    integer,   allocatable, dimension(:)        :: ndi_l           ! pop with  E(ndi_l,l) = E++
    integer,   allocatable, dimension(:)        :: nsi_l           ! pop with  E(ndi_l,l) = E+
    integer                                     :: n_l2e           ! nof CI states of symmetry l2e
    integer                                     :: cycle  !cycle number (time of evaluation of distribution)

!Redundant variables now after reformulating loops. Fix this.
    integer                 :: counter !for checking progress
    integer                 :: i, ijmax                !Index for energies
    integer                 :: l, l1max                !Index for angular momenta
    integer                 :: ic, ie, ie_max    !,ie_p        !Index for reformulated summations in terms of configuration number
    integer                 :: nof_points, nof_points2

    integer :: mod_dummy

!Wavefunction variables
    type(symmetry_ls2e), allocatable, dimension(:) :: w2e
    integer                 :: ir, ir1, ir2!, ir_skip            Wavefunction point index, i.e piont 1, 2, etc
  real(DPK), dimension(:), allocatable :: r          !Wavfefunction grid values
    real(DPK), dimension(:,:,:), allocatable:: p       !Wavefunction values, P_nl(r)
    real(DPK), dimension(:,:)  , allocatable:: prr, prr_ic, prr_icp, prr_work, f !2e radial distribution
    real(DPK), dimension(:,:)  , allocatable:: f00,f01,f02,f03,f11,f12,f13,f22,f23,f33 !2e radial distribution

    real(DPK) :: pop_ic
    complex(DPK):: pop_t_ie, work


    character(len=25) :: wf1efilename              !Wavefunction data file
    logical :: file_exists
    integer:: t1, t2, clock_rate, clock_max

!command line arguments
    character(len=100):: partial_l1, partial_l2, num_cycles ,rd2efilename
    integer:: l1_int, l2_int, num_cycles_int!, L_int


!get cmd arguments
!    call getarg(1, L_total)
    call getarg(1, partial_l1)
    call getarg(2, partial_l2)
    call getarg(3, num_cycles)
    read(partial_l1(1:1), "(i1)") l1_int
    read(partial_l2(1:1), "(i1)") l2_int
    read(num_cycles(1:2), "(i2)") num_cycles_int
    write(*,*) num_cycles_int, "num cycles from cmd line"
   !read(L_total(1:1), "(i1)") L_int





    nof_points = 250
    nof_points2 = 200
    mod_dummy = 2000/nof_points


    write(*,*) 'taking every', mod_dummy, 'th point'

    allocate (r(1:nof_points) )
    allocate( p(1:nof_points,1:ijmax,1:l1max) )

      loop_partial_waves: do l = 0, (l1max-1)

            write (wf1efilename, '(a,I1,a)') 'out/1ewavefunctions-',l,'.out'
            open(16, file = wf1efilename, status = 'old')

            loop_eigenstates: do i = 1, ijmax

              read(16,'(3E20.10)') r(1) , p(1, i, l+1)

                loop_wavefunction_grid: do ir = 2, 2000

                   if (mod(ir, mod_dummy) .eq. 0) then
                      read(16,'(3E20.10)') r(ir/mod_dummy) , p((ir/mod_dummy), i, l+1)
                  !     read(16,'(3E20.10)') r(ir) , p((ir), i, l+1)
                  !     write(*,*) r(ir)
                   else
                      read(16,'(3E20.10)')
                   end if
                end do loop_wavefunction_grid
                !loop_skip: do ir_skip = (nof_points+1), 2000
                !  read(16,'(3E20.10)')
                !end do loop_skip
             end do loop_eigenstates
             close(16)
          end do loop_partial_waves






          !Initialise 2e radial distribution matrix, prr = p(r1,r2)
          allocate(  prr(1:nof_points, 1:nof_points) )
          allocate(  prr_ic(1:nof_points, 1:nof_points) )
          allocate(  prr_icp(1:nof_points, 1:nof_points) )
          allocate(  prr_work(1:nof_points, 1:nof_points) )
          allocate(  f(1:nof_points, 1:nof_points) )
          allocate(  f00(1:nof_points, 1:nof_points) )
          allocate(  f01(1:nof_points, 1:nof_points) )
          allocate(  f02(1:nof_points, 1:nof_points) )
          allocate(  f03(1:nof_points, 1:nof_points) )
          allocate(  f11(1:nof_points, 1:nof_points) )
          allocate(  f12(1:nof_points, 1:nof_points) )
          allocate(  f13(1:nof_points, 1:nof_points) )
          allocate(  f22(1:nof_points, 1:nof_points) )
          allocate(  f23(1:nof_points, 1:nof_points) )
          allocate(  f33(1:nof_points, 1:nof_points) )

          prr      = 0.0_dpk
          prr_ic   = 0.0_dpk
          prr_icp  = 0.0_dpk
          prr_work = 0.0_dpk
          f = 0.0_dpk






          write(rd2efilename,"(a)") "rd2e/partial-waves/2f-partial-waves-di-00"
          open(19, file = rd2efilename)


          do ir1 = 1, nof_points2

            do ir2 = 1, nof_points2
                  read(19,'(3E20.10)')  dummie7, r(ir2), f00(ir1,ir2)
            end do
            read(19,'(3E20.10)')

          end do
          close(19)
          !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
          write(rd2efilename,"(a)") "rd2e/partial-waves/2f-partial-waves-di-01"
          open(19, file = rd2efilename)


          do ir1 = 1, nof_points2

            do ir2 = 1, nof_points2
                  read(19,'(3E20.10)')  dummie7, r(ir2), f01(ir1,ir2)
            end do
            read(19,'(3E20.10)')

          end do
          close(19)
          !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
          write(rd2efilename,"(a)") "rd2e/partial-waves/2f-partial-waves-di-02"
          open(19, file = rd2efilename)


          do ir1 = 1, nof_points2

            do ir2 = 1, nof_points2
                  read(19,'(3E20.10)')  dummie7, r(ir2), f02(ir1,ir2)
            end do
            read(19,'(3E20.10)')

          end do
          close(19)
          !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
          write(rd2efilename,"(a)") "rd2e/partial-waves/2f-partial-waves-di-03"
          open(19, file = rd2efilename)


          do ir1 = 1, nof_points2

            do ir2 = 1, nof_points2
                  read(19,'(3E20.10)')  dummie7, r(ir2), f03(ir1,ir2)
            end do
            read(19,'(3E20.10)')

          end do
          close(19)
          !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
          write(rd2efilename,"(a)") "rd2e/partial-waves/2f-partial-waves-di-11"
          open(19, file = rd2efilename)


          do ir1 = 1, nof_points2

            do ir2 = 1, nof_points2
                  read(19,'(3E20.10)')  dummie7, r(ir2), f11(ir1,ir2)
            end do
            read(19,'(3E20.10)')

          end do
          close(19)
          !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
          write(rd2efilename,"(a)") "rd2e/partial-waves/2f-partial-waves-di-12"
          open(19, file = rd2efilename)


          do ir1 = 1, nof_points2

            do ir2 = 1, nof_points2
                  read(19,'(3E20.10)')  dummie7, r(ir2), f12(ir1,ir2)
            end do
            read(19,'(3E20.10)')

          end do
          close(19)
          !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
          write(rd2efilename,"(a)") "rd2e/partial-waves/2f-partial-waves-di-13"
          open(19, file = rd2efilename)


          do ir1 = 1, nof_points2

            do ir2 = 1, nof_points2
                  read(19,'(3E20.10)')  dummie7, r(ir2), f13(ir1,ir2)
            end do
            read(19,'(3E20.10)')

          end do
          close(19)
          !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
          write(rd2efilename,"(a)") "rd2e/partial-waves/2f-partial-waves-di-22"
          open(19, file = rd2efilename)

          do ir1 = 1, nof_points2

            do ir2 = 1, nof_points2
                  read(19,'(3E20.10)')  dummie7, r(ir2), f22(ir1,ir2)
            end do
            read(19,'(3E20.10)')

          end do
          close(19)
          !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
          write(rd2efilename,"(a)") "rd2e/partial-waves/2f-partial-waves-di-23"
          open(19, file = rd2efilename)


          do ir1 = 1, nof_points2

            do ir2 = 1, nof_points2
                  read(19,'(3E20.10)')  dummie7, r(ir2), f23(ir1,ir2)
            end do
            read(19,'(3E20.10)')

          end do
          close(19)
          !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
          write(rd2efilename,"(a)") "rd2e/partial-waves/2f-partial-waves-di-33"
          open(19, file = rd2efilename)


          do ir1 = 1, nof_points2

            do ir2 = 1, nof_points2
                  read(19,'(3E20.10)')  dummie7, r(ir2), f33(ir1,ir2)
            end do
            read(19,'(3E20.10)')

          end do
          close(19)
          !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


          f = f00 + f01 + f02 +f03 +f11 +f12 +f13 +f22 +f23 +f33

          write(rd2efilename,"(a)") "rd2e/partial-waves/2f-di-total"
          open(19, file = rd2efilename, status = 'replace')

          do ir1 = 1, nof_points2
            do ir2 = 1, nof_points2
                  write(19,'(3E20.10)')  r(ir1), r(ir2), f(ir1,ir2)
            end do
            write(19,'(3E20.10)')

          end do
          close(19)


end program rd2e
