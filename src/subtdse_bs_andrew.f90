!     ft = A_t(t,spulse,e0,tau,omeg,cepd)
!  user supplied routine:
!  calculates the yprime 
!
!

!
!
! TDSE in the length gauge
!
!

!
! new version  
! 
!

SUBROUTINE tdse_v(t,y,ypr)             !nag
  !
  USE PRECISION, ONLY: dpk
  !  USE units,     ONLY: m_pi
  USE pulse      
  USE parameter_tdse_fxd
  USE deriv_tdse_fxd
  !
  IMPLICIT NONE
        !
  REAL(dpk)                         :: t
  REAL(dpk),         DIMENSION(neq) :: y
  REAL(dpk), TARGET, DIMENSION(neq) :: ypr

  !
  INTEGER                           :: i, k
  REAL(dpk)                         :: ft       ! field ( E(t) or A(t) )
  !ex!


  neval = neval + 1


  ! dy_r =   en * y_i 
  ! dy_i = - en * y_r 

  DO i = 1, ntot
     ypr(i)      =   dag(i) * y(i + ntot)
     ypr(i+ntot) = - dag(i) * y(i)
  END DO


  
  !
  ! non-diagonal part         ( V(t) * y(t) )
  !


   ft = A_t(t) 

  !  ft = A_t(t) + A_t(t + td)

  ! dy_r = - A(t) * v * y_r 
  ! dy_i = - A(t) * v * y_i 

  
  ! l = l_min = 0 

  !re(ypr) = re(ypr) - At * v * re(y)
  CALL dgemv ('t', n(2), n(1), -ft, dzr(1,1,1), nmax,       &  
                  & y(nsum(2)), 1, 1.0d0, ypr(1), 1)

  !im(ypr) = im(ypr) + At * v * im(y)
  CALL dgemv ('t', n(2), n(1), -ft, dzr(1,1,1), nmax,       &
                & y(ntot+nsum(2)), 1, 1.0d0, ypr(ntot+1), 1)


  ! 1 < l < l_max - 1

  DO  k = 2, lmax

     !re(ypr)
     CALL dgemv ('t', n(1+k), n(k), -ft, dzr(1,1,k), nmax,   &
          & y(nsum(k+1)), 1, 1.0d0, ypr(nsum(k)), 1)

     !im(ypr)
     CALL dgemv ('t', n(1+k), n(k), -ft, dzr(1,1,k), nmax,    &
              & y(ntot+nsum(k+1)), 1, 1.0d0, ypr(ntot+nsum(k)), 1)

     !re(ypr)
     CALL dgemv ('n', n(k), n(k-1), ft, dzr(1,1,k-1), nmax,  &
          &  y(nsum(k-1)), 1, 1.0d0, ypr(nsum(k)), 1)
  
     !im(ypr)
     CALL dgemv ('n', n(k), n(k-1), ft, dzr(1,1,k-1), nmax,   &
          & y(ntot+nsum(k-1)), 1, 1.0d0, ypr(ntot+nsum(k)), 1)
     
  ENDDO

  !re(ypr)
  CALL dgemv ('n', n(lmax+1), n(lmax), ft, dzr(1,1,lmax), nmax, &
       &  y(nsum(lmax)), 1, 1.0d0, ypr(nsum(lmax+1)), 1)

  !im(ypr)
  CALL dgemv ('n', n(lmax+1), n(lmax), ft, dzr(1,1,lmax), nmax,  &
       &  y(ntot+nsum(lmax)), 1, 1.0d0, ypr(ntot+nsum(lmax+1)), 1)
  
  yderiv => ypr
  

END SUBROUTINE tdse_v
!
!
! original version
!
!
SUBROUTINE tdse_v1(t,y,ypr)             !nag
  !
  USE PRECISION, ONLY: dpk
  USE pulse      
  USE parameter_tdse_fxd
  USE deriv_tdse_fxd
  !
  IMPLICIT NONE
        !
  REAL(dpk), TARGET, DIMENSION(neq) :: ypr
  REAL(dpk),         DIMENSION(neq) :: y
  !
  INTEGER                           :: i, k
  REAL(dpk)                         :: t 
  REAL(dpk)                         :: ft       ! field ( E(t) or A(t) )
  !ex!


  neval = neval + 1



  DO i = 1, ntot
     ypr(i)      =   dag(i) * y(i + ntot)
     ypr(i+ntot) = - dag(i) * y(i)
  END DO



  
  !
  ! non-diagonal part         ( V(t) * y(t) )
  !


  ft = A_t(t) 

  
  ! l = l_min = 0 

  !re(ypr) = re(ypr) - ft * v * im(y)
  CALL dgemv ('t', n(2), n(1), -ft, dzr(1,1,1), nmax,       &  
       & y(ntot+nsum(2)), 1, 1.0d0, ypr(1), 1)

  !im(ypr) = im(ypr) + ft * v * im(y)
  CALL dgemv ('t', n(2), n(1), ft, dzr(1,1,1), nmax,        &
       & y(nsum(2)), 1, 1.0d0, ypr(ntot+1), 1)


  ! 1 < l < l_max - 1

  DO  k = 2, lmax

     !re(ypr)
     CALL dgemv ('t', n(1+k), n(k), -ft, dzr(1,1,k), nmax,   &
          & y(ntot+nsum(k+1)), 1, 1.0d0, ypr(nsum(k)), 1)

     !im(ypr)
     CALL dgemv ('t', n(1+k), n(k), ft, dzr(1,1,k), nmax,    &
              & y(nsum(k+1)), 1, 1.0d0, ypr(ntot+nsum(k)), 1)

     !re(ypr)
     CALL dgemv ('n', n(k), n(k-1), -ft, dzr(1,1,k-1), nmax,  &
          &  y(ntot+nsum(k-1)), 1, 1.0d0, ypr(nsum(k)), 1)
  
     !im(ypr)
     CALL dgemv ('n', n(k), n(k-1), ft, dzr(1,1,k-1), nmax,   &
          & y(nsum(k-1)), 1, 1.0d0, ypr(ntot+nsum(k)), 1)
     
  ENDDO

  !re(ypr)
  CALL dgemv ('n', n(lmax+1), n(lmax), -ft, dzr(1,1,lmax), nmax, &
       &  y(ntot+nsum(lmax)), 1, 1.0d0, ypr(nsum(lmax+1)), 1)

  !im(ypr)
  CALL dgemv ('n', n(lmax+1), n(lmax), ft, dzr(1,1,lmax), nmax,  &
       &  y(nsum(lmax)), 1, 1.0d0, ypr(ntot+nsum(lmax+1)), 1)
  
  yderiv => ypr
  

END SUBROUTINE tdse_v1

!
!
! TDSE in the length gauge
!
!
SUBROUTINE tdse_l(t,y,ypr)             !nag
  !
  USE PRECISION, ONLY: dpk
  USE pulse      
  USE parameter_tdse_fxd
  USE deriv_tdse_fxd
  !
  IMPLICIT NONE
        !
  REAL(dpk), TARGET, DIMENSION(neq) :: ypr
  REAL(dpk),         DIMENSION(neq) :: y
  !
  INTEGER                           :: i, k
  REAL(dpk)                         :: t
  REAL(dpk)                         :: ft       !E(t) 
  !ex!


  neval = neval + 1


  ! dy_r =   en * y_i 
  ! dy_i = - en * y_r 

  DO i = 1, ntot
     ypr(i)      =   dag(i) * y(i + ntot)
     ypr(i+ntot) = - dag(i) * y(i)
  END DO

  
  !
  ! non-diagonal part         ( V(t) * y(t) )
  !
  
  ! l = l_min = 0 

  !re(ypr)
!  CALL dgemv ('t', n(2), n(1), ft, dzr(1,1,1), nmax,       &  
!       & y(ntot+nsum(2)), 1, 1.0d0, ypr(1), 1)

  !im(ypr)
!  CALL dgemv ('t', n(2), n(1), -ft, dzr(1,1,1), nmax,        &
!       & y(nsum(2)), 1, 1.0d0, ypr(ntot+1), 1)



  ft = E_t(t) 
  
  !
  ! z = r * cos(theta)
  !


  ! dy_r =   E(t) * z * y_i 
  ! dy_i = - E(t) * z * y_r 


  !re(ypr)
  CALL dgemv ('t', n(2), n(1), ft, dr(1,1,1), nmax,        &
       & y(ntot+nsum(2)), 1, 1.0d0, ypr(1), 1)

  !im(ypr)
  CALL dgemv ('t', n(2), n(1), -ft, dr(1,1,1), nmax,       &  
       & y(nsum(2)), 1, 1.0d0, ypr(ntot+1), 1)


  ! 1 < l < l_max - 1

  DO  k = 2, lmax


     ! <l|r|l+1>
     !re(ypr)   
     CALL dgemv ('t', n(1+k), n(k), ft, dr(1,1,k), nmax,    &
              & y(ntot+nsum(k+1)), 1, 1.0d0, ypr(nsum(k)), 1)
     !im(ypr)
     CALL dgemv ('t', n(1+k), n(k), -ft, dr(1,1,k), nmax,   &
          & y(nsum(k+1)), 1, 1.0d0, ypr(ntot+nsum(k)), 1)


     ! <l-1|r|l>
     !re(ypr)
     CALL dgemv ('n', n(k), n(k-1), ft, dr(1,1,k-1), nmax,   &
          & y(ntot+nsum(k-1)), 1, 1.0d0, ypr(nsum(k)), 1)

     !im(ypr)
     CALL dgemv ('n', n(k), n(k-1), -ft, dr(1,1,k-1), nmax,  &
          &  y(nsum(k-1)), 1, 1.0d0, ypr(ntot+nsum(k)), 1)
  
     
  ENDDO


  !re(ypr)
  CALL dgemv ('n', n(lmax+1), n(lmax), ft, dr(1,1,lmax), nmax,  &
       &  y(ntot+nsum(lmax)), 1, 1.0d0, ypr(nsum(lmax+1)), 1)

  !im(ypr)
  CALL dgemv ('n', n(lmax+1), n(lmax), -ft, dr(1,1,lmax), nmax, &
       &  y(nsum(lmax)), 1, 1.0d0, ypr(ntot+nsum(lmax+1)), 1)
  
  yderiv => ypr
  

END SUBROUTINE tdse_l
!
!
!
!
!
SUBROUTINE tdse_lv(t,y,ypr)             !nag
  !
  USE PRECISION, ONLY: dpk
  USE pulse      
  USE parameter_tdse_fxd
  USE deriv_tdse_fxd
  !
  IMPLICIT NONE
        !
  REAL(dpk), TARGET, DIMENSION(neq) :: ypr
  REAL(dpk),         DIMENSION(neq) :: y
  !
  INTEGER                           :: i, k
  REAL(dpk)                         :: t
  REAL(dpk)                         :: ft       !A(t) 
  !ex!


  neval = neval + 1





  ! dy_r =   en * y_i 
  ! dy_i = - en * y_r 

  DO i = 1, ntot
     ypr(i)      =   dag(i) * y(i + ntot)
     ypr(i+ntot) = - dag(i) * y(i)
  END DO

  
  !
  ! non-diagonal part         ( V(t) * y(t) )
  !
  
  ! l = l_min = 0 

  !re(ypr)
!  CALL dgemv ('t', n(2), n(1), ft, dzr(1,1,1), nmax,       &  
!       & y(ntot+nsum(2)), 1, 1.0d0, ypr(1), 1)

  !im(ypr)
!  CALL dgemv ('t', n(2), n(1), -ft, dzr(1,1,1), nmax,        &
!       & y(nsum(2)), 1, 1.0d0, ypr(ntot+1), 1)



  ft = A_t(t) 
  
  !
  ! z = r * cos(theta)
  !


  ! dy_r =   E(t) * z * y_i 
  ! dy_i = - E(t) * z * y_r 


  !re(ypr)
  CALL dgemv ('t', n(2), n(1), ft, dr(1,1,1), nmax,        &
       & y(ntot+nsum(2)), 1, 1.0d0, ypr(1), 1)

  !im(ypr)
  CALL dgemv ('t', n(2), n(1), -ft, dr(1,1,1), nmax,       &  
       & y(nsum(2)), 1, 1.0d0, ypr(ntot+1), 1)


  ! 1 < l < l_max - 1

  DO  k = 2, lmax


     ! <l|r|l+1>
     !re(ypr)   
     CALL dgemv ('t', n(1+k), n(k), ft, dr(1,1,k), nmax,    &
              & y(ntot+nsum(k+1)), 1, 1.0d0, ypr(nsum(k)), 1)
     !im(ypr)
     CALL dgemv ('t', n(1+k), n(k), -ft, dr(1,1,k), nmax,   &
          & y(nsum(k+1)), 1, 1.0d0, ypr(ntot+nsum(k)), 1)


     ! <l-1|r|l>
     !re(ypr)
     CALL dgemv ('n', n(k), n(k-1), ft, dr(1,1,k-1), nmax,   &
          & y(ntot+nsum(k-1)), 1, 1.0d0, ypr(nsum(k)), 1)

     !im(ypr)
     CALL dgemv ('n', n(k), n(k-1), -ft, dr(1,1,k-1), nmax,  &
          &  y(nsum(k-1)), 1, 1.0d0, ypr(ntot+nsum(k)), 1)
  
     
  ENDDO


  !re(ypr)
  CALL dgemv ('n', n(lmax+1), n(lmax), ft, dr(1,1,lmax), nmax,  &
       &  y(ntot+nsum(lmax)), 1, 1.0d0, ypr(nsum(lmax+1)), 1)

  !im(ypr)
  CALL dgemv ('n', n(lmax+1), n(lmax), -ft, dr(1,1,lmax), nmax, &
       &  y(nsum(lmax)), 1, 1.0d0, ypr(ntot+nsum(lmax+1)), 1)
  
  yderiv => ypr
  

END SUBROUTINE tdse_lv

!
!y(t) structure
!      
!
!     1.............................................,ntot,ntot+1,.....................2*ntot| 
!
!   <------------- real part ---------------------------><--------- Imag Part ------------->       
!
!  [yr_10,..yr_N0 | yr_11,...yr_N1, ...| yr_1L,...yr_NL [] yi_10,..yi_N0 | yi_11,...yi_N1,..| yi_1L,...yi_NL]
!
!
!  <--- L  = 0 ---><--- L  = 1 --->,..,<--- L  = Lm --->[] <--- L = 0 ---><--- L  = 1 --->,..,<--- L  = Lm --->
!

!eof
