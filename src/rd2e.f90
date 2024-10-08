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
    integer   :: ll1,ll2, nn1, nn2, nof_l_ncf
    real(DPK) :: p1, p2, p1p, p2p, p1p_bar, p2p_bar
    real(DPK) :: pop_t_ie, pop_ic

!Redundant variables now after reformulating loops. Fix this.
    integer                 :: counter !for checking progress
    integer                 :: i, ijmax                !Index for energies
    integer                 :: l, l1max                !Index for angular momenta
    integer                 :: ic, ie, ie_max    !,ie_p        !Index for reformulated summations in terms of configuration number
    integer                 :: nof_points, nof_points2
    real(dpk), dimension(:,:), pointer :: en1e        !nl,ns
    integer,   allocatable, dimension(:)        :: ndi_l           ! pop with  E(ndi_l,l) = E++
    integer,   allocatable, dimension(:)        :: nsi_l           ! pop with  E(ndi_l,l) = E+
    integer                                     :: n_l2e           ! nof CI states of symmetry l2e

    integer :: mod_dummy
!Wavefunction variables
    integer                 :: ir, ir1, ir2, ir_skip            !Wavefunction point index, i.e piont 1, 2, etc
    real(DPK), dimension(:), allocatable :: r          !Wavfefunction grid values
    real(DPK), dimension(:,:,:), allocatable:: p       !Wavefunction values, P_nl(r)
    real(DPK), dimension(:,:)  , allocatable:: prr, prr_ic, prr_icp, prr_work  !2e radial distribution
    real(DPK), dimension(:,:)  , allocatable:: work1, work2    !dummy array for 2e distribution summation

    character(len=25) :: wf1efilename              !Wavefunction data file
    logical :: file_exists

!configuration interaction variables
    type(symmetry_ls2e), allocatable, dimension(:) :: w2e

!open-mp
integer:: t1, t2, clock_rate, clock_max


!----------------------------Get number of states 1e from inp/h1e.inp -------------------------
    open(unit = 8, file = "inp/h1e.inp", status = 'old')

        !Dummie file read
        read(8, '(2f15.5,10x,a10)')           dummie1, dummie2
        read(8, '(a10,2f10.5,10x,a25)')       dummie3, dummie4, dummie5
        !read number of B-splines
        read(8, '(2I15,10x,a8)') ijmax, dummie6
        write(*,*) "#Number of b-splines = ", ijmax

    close(8)

        ijmax = ijmax - 2
        write(*,*) "#Number of eigenstates = ", ijmax
    write(*,*)
!------------------------Check wavefunction files exist. Retrive maximum angular momentum-----------

    l = 0
    write (wf1efilename, '(a,I1,a)') 'out/1ewavefunctions-',l,'.out'
    inquire(FILE = wf1efilename, EXIST = file_exists)

        if (file_exists) then
        write(*,*) "#Wavefunction files found"
        write(*,*) "#Finding number of partial wave files"
        write(*,*)
    else
        write(*,*) "#Could not find wavefunction files, 1ewavefunction-l.out."
        write(*,*) "#Terminating program"
        stop
    end if

    do while (file_exists)
        write (wf1efilename, '(a,I1,a)') 'out/1ewavefunctions-',l,'.out'
        inquire(FILE = wf1efilename, EXIST = file_exists)
        if(file_exists) then
            write(*,*) "#File found: ", wf1efilename
            l = l + 1
        else
            exit
        end if
    end do

    write(*,*)

    l1max = l
    write(*,*) "#Number of partial waves = l1max = ", l1max
    write(*,*) "#number of wavefunction points = ", np

!-------Read wavefunctions to p(np,ijmax,l1max). Tested 28/01/2019. Correct to this point--------



    nof_points = 2000
    nof_points2 = 125
    mod_dummy = 2000/nof_points


    write(*,*) 'taking every', mod_dummy, 'th point'

    allocate (r(1:nof_points) )
    allocate( p(1:nof_points,1:ijmax,1:l1max) )

      loop_partial_waves: do l = 0, (l1max-1)

            write (wf1efilename, '(a,I1,a)') 'out/1ewavefunctions-',l,'.out'
            open(16, file = wf1efilename, status = 'old')

            loop_eigenstates: do i = 1, ijmax

              read(16,'(3E20.10)') r(1) , p(1, i, l+1)

                loop_wavefunction_grid: do ir = 2, nof_points !2000

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




          !--------------------------R￼ead TDSE Coefficients and Configuration Interaction Coefficients-------------------------

          call input_tdse_fxd              ! tinp/tdse_bs_fxd_2e.inp
          call output_tdse_fxd             ! tout/tdse_bs_fxd_2e.out
          call read_target_energies(en1e)  ! read 1e energies (in Ryd)

          call read_w2e_ct(w2e,lmax)

          do l = 0, lmax
             call read_w2e_ci(w2e(l),l)     !reads coeffs Phi_j = \sum_i v_{ij} Phi^{(0)}_{i}
          end do

          !-----------------------------------Apply summation equation-----------------------------------

          !Initialise 2e radial distribution matrix, prr = p(r1,r2)
          allocate(  prr(1:nof_points, 1:nof_points) )
          allocate(  prr_ic(1:nof_points, 1:nof_points) )
          allocate(  prr_icp(1:nof_points, 1:nof_points) )
          allocate(  prr_work(1:nof_points, 1:nof_points) )
          allocate(  work1(1:nof_points, 1:nof_points) )
          allocate(  work2(1:nof_points, 1:nof_points) )

          prr      = 0.0_dpk
          prr_ic   = 0.0_dpk
          prr_icp  = 0.0_dpk
          prr_work = 0.0_dpk

          work1 = 0.0_dpk
          work2 = 0.0_dpk

          counter  = 0
          !ijmax = 60
          !lmax = 7
          !Nmax = 5998 Need to import this from somewhere

          write(*,*) "#Number of L=0 configurations in sum/prime sums = ", w2e(0)%ncf
          write(*,*) "#Number of L=1 configurations in sum/prime sums = ", w2e(1)%ncf
          write(*,*) "#Number of L=2 configurations in sum/prime sums = ", w2e(2)%ncf
          write(*,*) "#Number of L=3 configurations in sum/prime sums = ", w2e(3)%ncf




!Find ionisation thresholds


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



       call system_clock ( t1, clock_rate, clock_max )


!Begin radial distribution loops
loop_L: do l = 0, 0!lmax

  write(*,*) "Angular Symmetry L = ", l
  !if ((l .eq. 1) .or. (l .eq. 3)) then
  !     cycle
  !endif

  nof_l_ncf = w2e(l)%ncf

  !Set maximum number of 2e energies to sum over for various L symmetries
  if (l .eq. 0) then
    ie_max = 834
  else if (l .eq. 1) then
    ie_max = 1182
  else if (l .eq. 2) then
    ie_max = 1314
  else if (l .eq. 3) then
    ie_max = 1053
  end if
  write(*,*) "#ie max = ", ie_max

  loop_total_energy: do ie = 1, 1 !ndi_l(l), ie_max
    !  if ((ie .eq. 1) .and. (l .eq. 0)) then
    !          cycle
    !  endif
    write(*,*) "Starting energy level", ie

    pop_t_ie = abs( w2e(l)%ct(ie))**2


    !  !$omp parallel private(ic, ll1, ll2, nn1, nn2, p1, p2, p1p, p1p_bar, p2p, p2p_bar, pop_ic) reduction(+: work1, work2, prr_ic, prr_icp)
    !  !$omp do schedule(static)

      loop_config_variables: do ic = 1, nof_l_ncf !sum from configuration 1 to max number of configurations, for the total  L
        pop_ic = w2e(l)%cv(ie,ic)
        ll1 = w2e(l)%l1(ic) + 1
        ll2 = w2e(l)%l2(ic) + 1
        nn1 =  w2e(l)%n1(ic)
        nn2  = w2e(l)%n2(ic)

        work1 = 0.0_dpk
        loop_r1: do ir1 = 1, nof_points2
          p1 = p( ir1, nn1,  ll1 )
          loop_r2: do ir2 = ir1, nof_points2
            work1(ir1,ir2) =  (p1 * p( ir2, nn2,  ll2 ))*pop_ic
          end do loop_r2
        end do loop_r1

        work2 = 0.0_dpk
        loop_r1_prime: do ir1 = 1, nof_points2
          p1p     = p( ir1, nn1,  ll1 )
          p1p_bar = p( ir1, nn1,  ll2 )
          loop_r2_prime: do ir2 = ir1, nof_points2
            work2(ir1,ir2) =  (p1p * p( ir2, nn2,  ll2 ) +  p1p_bar * p( ir2, nn2,  ll1 ))*pop_ic
          end do loop_r2_prime
        end do loop_r1_prime

!        if ((ll1 .eq. ll2) .and. (nn1 .eq. nn2)) then
!          work1 = work1*0.5
!          work2 = work2*0.5
!        end if

        prr_ic  = prr_ic  + work1
        prr_icp  = prr_icp  + work2

      end do loop_config_variables

    !  !$OMP END DO NOWAIT
    !  !$OMP END PARALLEL



      !Multiply unprimed and primed sums to get radial distribution(to be multiplied by |Cnl(t)|^2 )
      prr_work = 0.0_dpk
      loop_multiply_distributions_r1: do ir1 = 1, nof_points2
        loop_multiply_distributions_r2: do ir2 = ir1, nof_points2
          prr_work(ir1, ir2) = prr_ic(ir1, ir2) * prr_icp(ir1, ir2)
        end do loop_multiply_distributions_r2
      end do loop_multiply_distributions_r1

      !TDSE coefficients
      prr = prr + (prr_work * pop_t_ie)

      write(*,*) "Energy level", ie, "done"

    end do loop_total_energy

end do loop_L

call system_clock ( t2, clock_rate, clock_max )
write ( *, * ) 'Elapsed real time = ', real ( t2 - t1 ) / real ( clock_rate )


 !Mirror across diagonal and write to file
 open(19, file = "radial-distributions/ground-state-2000-with-norm.out", status = 'replace')

         do ir1 = 1, nof_points2
             do ir2 = 1, nof_points2

               if ((ir2 .ge. ir1)) then

                 if (prr(ir1,ir2) .lt. 0) then
                   write(19,'(3E20.10)')  r(ir1)*2.645, r(ir2)*2.645, 0.00000 !SCALED.ATOMIC.UNITS TO nm
                 else
                   write(19,'(3E20.10)')  r(ir1)*2.645, r(ir2)*2.645, prr(ir1,ir2) !SCALED.ATOMIC.UNITS TO nm
                 end if

               else

                 if (prr(ir2,ir1) .lt. 0) then
                   write(19,'(3E20.10)')  r(ir1)*2.645, r(ir2)*2.645, 0.00000 !SCALED.ATOMIC.UNITS TO nm
                 else
                   write(19,'(3E20.10)')  r(ir1)*2.645, r(ir2)*2.645, prr(ir2,ir1) !SCALED.ATOMIC.UNITS TO nm
                 end if


               end if
             end do
             write(19,'(3E20.10)')

         end do

 close(19)



end program rd2e
