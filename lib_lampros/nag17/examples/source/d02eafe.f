*     D02EAF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          N, IW
      PARAMETER        (N=3,IW=(12+N)*N+50)
*     .. Local Scalars ..
      DOUBLE PRECISION TOL, X, XEND
      INTEGER          I, IFAIL, J
*     .. Local Arrays ..
      DOUBLE PRECISION W(IW), Y(N)
*     .. External Subroutines ..
      EXTERNAL         D02EAF, FCN
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D02EAF Example Program Results'
      XEND = 10.0D0
      DO 20 J = 3, 4
         TOL = 10.0D0**(-J)
         WRITE (NOUT,*)
         WRITE (NOUT,99999) 'Calculation with TOL =', TOL
         X = 0.0D0
         Y(1) = 1.0D0
         Y(2) = 0.0D0
         Y(3) = 0.0D0
         WRITE (NOUT,*) '    X         Y(1)         Y(2)         Y(3)'
         WRITE (NOUT,99998) X, (Y(I),I=1,N)
         IFAIL = 0
*
         CALL D02EAF(X,XEND,N,Y,TOL,FCN,W,IW,IFAIL)
*
         WRITE (NOUT,99998) X, (Y(I),I=1,N)
         IF (TOL.LT.0.0D0) WRITE (NOUT,*) ' Range too short for TOL'
   20 CONTINUE
      STOP
*
99999 FORMAT (1X,A,D8.1)
99998 FORMAT (1X,F7.2,3F13.5)
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
