*     D02EGF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          N, IW
      PARAMETER        (N=3,IW=(12+N)*N+50)
*     .. Local Scalars ..
      DOUBLE PRECISION HMAX, TOL, VAL, X, XEND
      INTEGER          I, IFAIL, M
*     .. Local Arrays ..
      DOUBLE PRECISION W(IW), Y(N)
*     .. External Subroutines ..
      EXTERNAL         D02EGF, FCN
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D02EGF Example Program Results'
      XEND = 10.0D0
      M = 1
      HMAX = 0.0D0
      VAL = 0.9D0
      DO 20 I = 3, 4
         TOL = 10.0D0**(-I)
         WRITE (NOUT,*)
         WRITE (NOUT,99998) 'Calculation with TOL =', TOL
         X = 0.0D0
         Y(1) = 1.0D0
         Y(2) = 0.0D0
         Y(3) = 0.0D0
         IFAIL = 0
*
         CALL D02EGF(X,XEND,N,Y,TOL,HMAX,M,VAL,FCN,W,IW,IFAIL)
*
         WRITE (NOUT,99999) ' Y(1)-VAL changes sign at X = ', X
         IF (TOL.LT.0.0D0) WRITE (NOUT,*) ' Range too short for TOL'
   20 CONTINUE
      STOP
*
99999 FORMAT (1X,A,F6.3)
99998 FORMAT (1X,A,D8.1)
      END
*
      SUBROUTINE FCN(T,Y,F)
*     .. Parameters ..
      INTEGER        N
      PARAMETER      (N=3)
*     .. Scalar Arguments ..
      DOUBLE PRECISION T
*     .. Array Arguments ..
      DOUBLE PRECISION F(N), Y(N)
*     .. Executable Statements ..
      F(1) = -0.04D0*Y(1) + 1.0D4*Y(2)*Y(3)
      F(2) = 0.04D0*Y(1) - 1.0D4*Y(2)*Y(3) - 3.0D7*Y(2)*Y(2)
      F(3) = 3.0D7*Y(2)*Y(2)
      RETURN
      END
