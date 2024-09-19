!    bsci2e.f90 : inactive, not finished
PROGRAM bsci2e
  !
  USE PRECISION, only: dpk
  USE bs1e_param

  !..................
  IMPLICIT NONE
  
  CHARACTER(len=255)  ::  argument
  INTEGER             ::  len
  INTEGER             ::  ier
  REAL                ::  arg_value
  
  !
  REAL(dpk)           :: v_0
  REAL(dpk)           :: z_n
  REAL(dpk)           :: r_b
  REAL(dpk)           :: n_b
  ! I/O
  INTEGER NOUT 
  INTEGER :: i, arg_count
  CHARACTER(len=100) :: arg_string, value_string
  LOGICAL :: v_set, zn_set, rb_set, nb_set
  
  ! Initialize default values
  v_0 = -1.0_dpk
  z_n = -1.0_dpk
  r_b = -1.0_dpk
  n_b = -1.0_dpk
  v_set = .false.; zn_set = .false.; rb_set = .false.; nb_set = .false.

  ! Get the number of command-line arguments
  arg_count = command_argument_count()

  ! Parse command-line arguments
  do i = 1, arg_count
     call get_command_argument(i, arg_string)

     ! Check for the -v flag
     if (arg_string == '-v') then
        call get_command_argument(i+1, value_string)
        read(value_string, *) v_0
        v_set = .true.
     ! Check for the -zn flag
     elseif (arg_string == '-zn') then
        call get_command_argument(i+1, value_string)
        read(value_string, *) z_n
        zn_set = .true.
     ! Check for the -rb flag
     elseif (arg_string == '-rb') then
        call get_command_argument(i+1, value_string)
        read(value_string, *) r_b
        rb_set = .true.
     ! Check for the -nb flag
     elseif (arg_string == '-nb') then
        call get_command_argument(i+1, value_string)
        read(value_string, *) n_b
        nb_set = .true.
     end if
  end do

  ! Ensure that all required arguments are set
  if (.not. v_set .or. .not. zn_set .or. .not. rb_set .or. .not. nb_set) then
     print *, "Usage: ./Rbspci2e -v <value> -zn <value> -rb <value> -nb <value>"
     stop
  end if

  ! Print the parsed values to verify
  WRITE(*,*) '# bspci2e:      value for  -v =', v_0
  WRITE(*,*) '# bspci2e:      value for -zn =', z_n
  WRITE(*,*) '# bspci2e:      value for -rb =', r_b
  WRITE(*,*) '# bspci2e:      value for -nb =', n_b
  
  ! Call the input_read_write routine
  CALL input_read_write(v_0, z_n, r_b, INT(n_b))
  
  nout  = 16  
  OPEN(nout, file='log/bsci2e.log')
  WRITE(*,*) '@ main program bspci2e end.'

END PROGRAM bsci2e