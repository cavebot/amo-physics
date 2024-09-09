*     E02GBF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          N, MPLMAX, IE, IW
      PARAMETER        (N=4,MPLMAX=12,IE=N,IW=3*MPLMAX+N*N+5*N+(N+1)
     +                 *(N+2)/2)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION EL1N, T, XI
      INTEGER          I, IFAIL, IPRINT, K, L, M, MXS
*     .. Local Arrays ..
      DOUBLE PRECISION E(IE,MPLMAX), F(MPLMAX), W(IW), X(N)
      INTEGER          IWORK(MPLMAX)
*     .. External Subroutines ..
      EXTERNAL         E02GBF, MONIT
*     .. Intrinsic Functions ..
      INTRINSIC        DBLE
*     .. Executable Statements ..
      WRITE (NOUT,*) 'E02GBF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) M
      L = M
      IF (M.GT.0 .AND. M+L.LE.MPLMAX) THEN
         DO 20 I = 1, M
            READ (NIN,*) T, F(I)
            XI = 0.1D0*DBLE(I-1)
            E(1,I) = 1.0D0
            E(2,I) = T
            E(3,I) = T*T
            E(4,I) = T*T*T
            E(1,M+I) = 0.0D0
            E(2,M+I) = 1.0D0
            E(3,M+I) = 2.0D0*T
            E(4,M+I) = 3.0D0*T*T
            F(M+I) = 0.0D0
   20    CONTINUE
         DO 40 I = 1, N
            X(I) = 0.0D0
   40    CONTINUE
         MXS = 50
*        * Set IPRINT=1 to obtain output from MONIT at each iteration *
         IPRINT = 0
         IFAIL = 1
*
         CALL E02GBF(M,N,M+L,E,IE,F,X,MXS,MONIT,IPRINT,K,EL1N,IWORK,W,
     +               IW,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,99999) 'IFAIL = ', IFAIL
      END IF
      STOP
*
99999 FORMAT (1X,A,I2)
      END
*
      SUBROUTINE MONIT(N,X,NITER,K,EL1N)
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Scalar Arguments ..
      DOUBLE PRECISION EL1N
      INTEGER          K, N, NITER
*     .. Array Arguments ..
      DOUBLE PRECISION X(N)
*     .. Executable Statements ..
      WRITE (NOUT,*)
      WRITE (NOUT,99999) 'Results at iteration ', NITER
      WRITE (NOUT,*) 'X-values'
      WRITE (NOUT,99998) X
      WRITE (NOUT,99997) 'Norm of residuals =', EL1N
      RETURN
*
99999 FORMAT (1X,A,I5)
99998 FORMAT (1X,4F15.4)
99997 FORMAT (1X,A,D12.5)
      END
