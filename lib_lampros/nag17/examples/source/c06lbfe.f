*     C06LBF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          MMAX
      PARAMETER        (MMAX=512)
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION B, EPSTOL, EXACT, FINV, PSERR, SIGMA, SIGMA0, T
      INTEGER          IFAIL, J, M
*     .. Local Arrays ..
      DOUBLE PRECISION ACOEF(MMAX), ERRVEC(8)
*     .. External Subroutines ..
      EXTERNAL         C06LBF, C06LCF
*     .. External Functions ..
      COMPLEX*16       F
      EXTERNAL         F
*     .. Intrinsic Functions ..
      INTRINSIC        ABS, EXP, DBLE, SINH
*     .. Executable Statements ..
      WRITE (NOUT,*) 'C06LBF Example Program Results'
      SIGMA0 = 3.0D0
      EPSTOL = 0.00001D0
      SIGMA = 0.0D0
      B = 0.0D0
      IFAIL = 0
*
*     Compute inverse transform
      CALL C06LBF(F,SIGMA0,SIGMA,B,EPSTOL,MMAX,M,ACOEF,ERRVEC,IFAIL)
*
      WRITE (NOUT,*)
      WRITE (NOUT,99999) 'No. of coefficients returned by C06LBF =', M
      WRITE (NOUT,*)
      WRITE (NOUT,*)
     +  '                 Computed          Exact      Pseudo'
      WRITE (NOUT,*)
     +  '         t           f(t)           f(t)       error'
      WRITE (NOUT,*)
*
*     Evaluate inverse transform for different values of t
      DO 20 J = 0, 5
         T = DBLE(J)
*
         CALL C06LCF(T,SIGMA,B,M,ACOEF,ERRVEC,FINV,IFAIL)
*
         EXACT = SINH(3.0D0*T)
         PSERR = ABS(FINV-EXACT)/EXP(SIGMA*T)
         WRITE (NOUT,99998) T, FINV, EXACT, PSERR
   20 CONTINUE
      STOP
*
99999 FORMAT (1X,A,I6)
99998 FORMAT (1X,1P,D10.2,2D15.4,D12.1)
      END
*
      COMPLEX*16     FUNCTION F(S)
*     .. Scalar Arguments ..
      COMPLEX*16                S
*     .. Executable Statements ..
      F = 3.0D0/(S**2-9.0D0)
      RETURN
      END
