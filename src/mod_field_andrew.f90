MODULE pulse
  !
  USE PRECISION
  !
  IMPLICIT NONE
  !
  PUBLIC
  !pulse!

  REAL(dpk)                           :: e0
  REAL(dpk)                           :: w0
  REAL(dpk)                           :: tau0
  REAL(dpk)                           :: tau,tg,chirp
  REAL(dpk)                           :: phi
  REAL(dpk)                           :: dw, gvd
  CHARACTER(len=10)                   :: pulseType
  INTEGER                             :: n_modes
  REAL(dpk), ALLOCATABLE, DIMENSION(:):: e_i, h_i
  REAL(dpk), ALLOCATABLE, DIMENSION(:):: w_i
  REAL(dpk), ALLOCATABLE, DIMENSION(:):: tau_i
  REAL(dpk), ALLOCATABLE, DIMENSION(:):: p_i
  REAL(dpk), ALLOCATABLE, DIMENSION(:):: c_i
  integer,   ALLOCATABLE, DIMENSION(:):: n_i
  REAL(dpk)                           :: start_time

CONTAINS


  SUBROUTINE READ_FIELD
    !
    USE units, ONLY:i0_td, enau, m_pi,t_fs
    USE io,    only:ninp
    !
    IMPLICIT NONE
    !
    CHARACTER(LEN=25)      :: field_file = "tinp/pulse.inp"
    REAL(dpk)              :: i0                   ! e0 = sqrt(i0): electric field
    REAL(dpk)              :: it_av
    INTEGER                :: i
    REAL(dpk)              :: ft_min
    !
    WRITE(*,*) '# opening field file :', field_file
    !

    ! CALL getarg(1, argv)              ! peak intensity
    ! READ(argv,*)   i0                 ! in SI 
    ! CALL getarg(2, argv)              ! photon frequency
    ! READ(argv,*)   w0              ! in SI
    ! CALL getarg(3, argv)              ! nof cycles
    ! READ(argv,*)   cycles             !  


    OPEN( ninp, file=TRIM(field_file),status='old')
    READ( ninp, * ) n_modes
    READ( ninp, * ) pulsetype ! pulse shape ('agauss', 'acos2' 'acos2flat', 'asin2', 'esin2','composite' )
    READ( ninp, * ) i0 
    READ( ninp, * ) w0           !T0 = 2*pi/w0
    READ( ninp, * ) start_time   ! number of tau
    READ( ninp, * ) tau          ! tau in fs
    READ( ninp, * ) phi          ! temporal phase

      
    !       
    ALLOCATE( n_i(ABS(n_modes)) )
    ALLOCATE( h_i(ABS(n_modes)) )
    ALLOCATE( p_i(ABS(n_modes)) )
    ALLOCATE( c_i(ABS(n_modes)) )


    n_i = 0
    h_i = 0.0
    p_i = 0.0
    c_i = 0.0
    IF( n_modes == 1 ) THEN

       n_i(1) = 1
       h_i(1) = 1.0
       
    ELSE

       READ(ninp,*) dw           ! bandwidth in eV
       READ(ninp,*) n_i
       READ(ninp,*) h_i
       READ(ninp,*) p_i          ! overwritten in the current version later on given the chirp
       READ(ninp,*) c_i          ! overwritten in the current version later on given the chirp
       
    ENDIF

    
    !    DO i = 1, n_modes
    !       READ(ninp,*)  n_i(i), h_i(i), p_i(i), c_i(i)
    !    ENDDO

    
    PRINT*, "mod_field::        n_modes =   ", n_modes
    PRINT*, "mod_field::     |n_modes| >= 1."

    CLOSE(ninp)

    !
    

    ALLOCATE( tau_i(ABS(n_modes)) )
    ALLOCATE( e_i(ABS(n_modes)) )
    ALLOCATE( w_i(ABS(n_modes)) )


    ! convert to a.u.
    tau  =  tau/t_fs                       ! T_P : pulse duration  (a.u.)    
    w0   =  w0/enau                        ! Wph :     eV -->      (a.u.)
    e0    = SQRT(i0/i0_td)                 ! E_o :  W/cm^2 -->     (a.u.)    
    tau0  = 2.0_dpk * m_pi/w0              ! T_0 : cycle period of the fundamental  (a.u.)

    

    w_i   = w0 * n_i
    tau_i = tau0/n_i
    e_i   = e0 * SQRT(h_i)                ! must be sum(hw_i) = 1       


    
    !    tau  =  cycles  * tau0                ! T_P : pulse duration  (a.u)
    !    it_av  = SUM(e_i2)
    !    e_i2  = e_i2/it_av


    IF(n_modes > 1) THEN   
       
       !       IF(dw < 0.0_dpk) THEN

          dw   = dw/enau                       ! dw  :     eV -->      (a.u.)
          
          ft_min = dw*tau/(4.0_dpk*LOG(2.0_dpk))
          
          IF(ft_min.GT.1.0_dpk) THEN
             
             chirp = SQRT(ft_min - 1.0_dpk) 
             gvd = chirp * 4.0_dpk * LOG(2.0_dpk)/dw**2
             
             PRINT*,'   gvd = ', gvd
             PRINT*,' chirp = ', chirp
             
          ELSE
             PRINT*,'dw*tau not compatible for Gaussian pulse: dw*tau > 4*ln2 = 2.7725'
             PRINT*,'dw*tau not compatible for Gaussian pulse: dw*tau = ', dw*tau    
             STOP          
          ENDIF
          
          c_i = chirp * 2.0_dpk*LOG(2.0_dpk)/(tau*tau)
          p_i = phi - 0.5_dpk * ATAN(chirp)
          
          !      ENDIF
    ENDIF
    
	
    PRINT*,dw
    PRINT*,tau,tau0    
    WRITE(*,'(a50,3E15.8)') '& i0 (au, W/cm^2) = ', e0**2, e0**2*i0_td
    WRITE(*,'(a50,10I5)')   '&      harmonics compositions = ', n_modes
    PRINT*,n_i
    PRINT*,e_i
    PRINT*,tau_i
    PRINT*,c_i
    PRINT*,p_i

    
    WRITE(*,*) '#  field file read done.'

    !
  END SUBROUTINE READ_FIELD
!
!
!

  SUBROUTINE SAVE_FIELD!!(tsteps)
    !
    use io
    use units
    !
    implicit none
    !
    REAL(dpk)              :: t, ti, tf, dt, tsteps
    INTEGER                :: n, nout_t, dim_t
    CHARACTER(LEN=25)      :: field_file     = "tout/pulse.out"
    CHARACTER(LEN=25)      :: intensity_file = "tout/intensity.out"

    !
    
    ti     = tau * start_time !  tf     = +tau * start_time
    dim_t  = 2 * INT(ABS(ti)/tau_i(1)) + 1

    tsteps = 10
    nout_t = dim_t * tsteps 
    
    dt     = tau_i(1)/tsteps 
    tf     = ti + nout_t * dt 

    OPEN( nout, file=trim(field_file))
    WRITE( nout,  '(a30,1P,2E20.5)') '&                       E0 = ', e0,e_i(1)                   ! a.u.
    WRITE( nout,  '(a30,1P,2E20.5)') '& intensity             i0 = ', e_i(1)**2, e_i(1)**2*i0_td  ! a.u., SI
    WRITE( nout,  '(a30,1P,2E20.5)') '&                        w = ', w_i(1), w_i(1)*enau         ! a.u., SI
    WRITE( nout,  '(a30,1P,2E20.5)') '& bandwidth             dw = ', dw,    dw*enau
    WRITE( nout,  '(a30,1P,4E20.5)') '& initial/final time ti/tf = ', ti, tf,   ti*t_fs, tf*t_fs 
    WRITE( nout,  '(a30,1P,2E20.5)') '& pulse period        tau0 = ', tau0,  tau0*t_fs
    WRITE( nout,  '(a30,1P,2E20.5)') '& pulse           tau_i(1) = ', tau_i(1),tau_i(1)*t_fs 
    WRITE( nout,  '(a30,1P,2E20.5)') '& pulse duration       tau = ', tau,   tau*t_fs 
    WRITE( nout,  '(a30,1P,1E20.5)') '&                      phi = ', phi
    WRITE( nout,  '(a30,a25)'    ) '& pulsetype            shape = ', pulsetype
    WRITE( nout,  '(a30,2i20)'   ) '& nof points          nout_t = ', nout_t, dim_t
    WRITE( nout,  '(a30,1P,1E20.5)') '&                       dt = ', dt
    WRITE( nout,  '(a1,3a20)'    ) '&','t(au)', 'A(t)', 'E(t)'
    
    t = ti

    DO n = 1, nout_t

       WRITE(nout,'(4E25.14)') t, E_t(t), A_t(t), envelope_t(t) 

       t = t + dt       
    ENDDO    
    CLOSE(nout)

    OPEN( nout, file=TRIM(intensity_file))
    WRITE( nout,  '(a30,1P,2E20.5)') '&                       E0 = ', e0,e_i(1)                   ! a.u.
    WRITE( nout,  '(a30,1P,2E20.5)') '& intensity             i0 = ', e_i(1)**2, e_i(1)**2*i0_td  ! a.u., SI
    WRITE( nout,  '(a30,1P,2E20.5)') '&                        w = ', w_i(1), w_i(1)*enau         ! a.u., SI
    WRITE( nout,  '(a30,1P,2E20.5)') '& bandwidth             dw = ', dw,    dw*enau
    WRITE( nout,  '(a30,1P,4E20.5)') '& initial/final time ti/tf = ', ti, tf,   ti*t_fs, tf*t_fs 
    WRITE( nout,  '(a30,1P,2E20.5)') '& pulse period        tau0 = ', tau0,  tau0*t_fs
    WRITE( nout,  '(a30,1P,2E20.5)') '& pulse           tau_i(1) = ', tau_i(1),tau_i(1)*t_fs 
    WRITE( nout,  '(a30,1P,2E20.5)') '& pulse duration       tau = ', tau,   tau*t_fs 
    WRITE( nout,  '(a30,1P,1E20.5)') '&                      phi = ', phi
    WRITE( nout,  '(a30,a25)'    ) '& pulsetype          shape = ', pulsetype
    WRITE( nout,  '(a30,2i20)'   ) '& nof points        nout_t = ', nout_t, dim_t
    WRITE( nout,  '(a30,1P,1E20.5)') '&                       dt = ', dt
    WRITE( nout,  '(a1,3a20)'    ) '&','t(au)', 'A(t)', 'E(t)'



    
    t = ti

    DO n = 1, nout_T
       
       WRITE(nout,'(4E25.14)') t, E_t(t)**2, A_t(t)**2, envelope_t(t)**2 

       t = t + dt       
    ENDDO    
    CLOSE(nout)

    
    WRITE(*,*) "# pulse::save_field:   pulse saved at ", field_file


  END SUBROUTINE SAVE_FIELD

  !
  !--------------------------------------------------------------------
  !
  !     ..
  !
  !  Purpose
  !  =======
  !
  !  POTENTIAL_AMPLI returns the value at time T of the vector potential A( T )
  !  coresponding to the physical field E( T ) and defined as :
  !
  !     E( T ) = - D / DT A( T )  
  !  
  !  with 
  !
  !     A( T ) = E0 / W0 * F( T, TAU ) * COS( W0 * T + PHI )
  !
  !  where F( T, TAU ) is the envelope of the pulse, W0 the photon energy
  !  and PHI the initial phase.
  !
  
  !    REAL(DPK) FUNCTION A_T( T, PULSETYPE, E0, TAU, W0, PHI )
  !      REAL(DPK)             E0, TAU, PHI, W0 
  !      CHARACTER(DPK)        PULSETYPE
  
  REAL(DPK) FUNCTION A_T(T)
    !
    USE PRECISION
    ! USE UNITS
    IMPLICIT NONE
    !
    REAL(DPK)   :: t
    REAL(dpk)   :: alpha, envelope
    !
    !    INTEGER                 :: nt
    !
    !    INTEGER                 :: nder, ifail    
    !    REAL(dpk)               :: dt
    !    REAL(dpk), DIMENSION(14):: der,  erest       
    !    nt   = 40
    !    nder = 1
    !    dt = (2*m_pi/w_i(1))/nt
    
    !    CALL d04aaf(t,1,dt,der,erest,e_t,ifail)
    !    a_t = der(1)
    !    der(1)


    !salpha = SQRT(alpha)*0.311    
    !a_t = (COS(salpha*t)**20/w0) * SUM( e_i2 * SIN( ( w_i + 0.000016*t)*t + p_i)/n_i )    
    !a_t = (EXP(-alpha*t*t)/w0) * SUM( e_i2 * SIN( ( w_i + 0.000016*t)*t + p_i)/n_i )

    alpha    = 2.0_dpk*LOG(2.0_dpk)
    envelope = EXP(- alpha * (t/tau)*(t/tau)) / w0
    a_t      =  envelope * SUM(e_i*SIN(w_i*t + c_i*t*t + p_i)/n_i )
    
    RETURN
    !
  END FUNCTION A_T


    REAL(DPK) FUNCTION ENVELOPE_T(T)
    !
      USE PRECISION
      IMPLICIT NONE
      !
      REAL(DPK)   :: t
      REAL(dpk)   :: alpha, envelope
      !
      alpha = 2.0_dpk*LOG(2.0_dpk)
      envelope_t = e_i(1) * EXP(- alpha*(t/tau)*(t/tau))/w_i(1)
      RETURN
    END FUNCTION ENVELOPE_T
    
  

  REAL(DPK) FUNCTION E_T(T)
    !
    USE PRECISION
    USE UNITS
    IMPLICIT NONE
    !
    REAL(DPK)   t
    INTEGER                 :: nt
    !
    INTEGER                 :: nder, ifail    
    REAL(dpk)               :: dt
    REAL(dpk), DIMENSION(14):: der,  erest
    
    nt   = 40
    nder = 1
    dt = (2*m_pi/w_i(1))/nt
    
    CALL d04aaf(t,1,dt,der,erest,a_t,ifail)

    e_t = der(1)
    !der(1)

    !    e_t =     e_i2(1) *  COS(w_i(1)*t+p_i(1))  !EXP(-0.5_dpk * (t*t / tau_i2) )  
    !    SUM( e_i2 * EXP(-0.5_dpk*(t*t/tau_i2) )  * COS(w_i*t+p_i) )
    RETURN
  END FUNCTION E_T



  !     ..
  !
  !  Purpose
  !  =======
  !
  !  E_T returns the value of an electromagnetic field E( T )
  !  defined as :
  !
  !     E( T ) = - D / DT A( T )
  !
  !  where A( T ) is defined as    
  !
  !  A( T ) = E0 / W0 * F( T, TAU ) * COS( W0 * T + PHI ) 
  !  
  !  and where F( T, TAU ) represents the shape of the pulse and is specified by
  !  PULSETYPE.
  !
  !  Available Shapes:
  !
  !     - Gauss  (Ok)
  !     - CosSqr (Ok)
  !     - CosSqF (Ok)
  !

  !  REAL(DPK) FUNCTION E_T( T, PULSETYPE, E0, TAU, W0, PHI)
  !REAL(DPK)         :: E0, TAU, PHI, W0 
  !CHARACTER(LEN=6)     ::PULSETYPE

  END MODULE pulse
!EOF!
