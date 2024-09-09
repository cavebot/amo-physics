*     D02CAF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          N
      PARAMETER        (N=3)
*     .. Local Scalars ..
      DOUBLE PRECISION PI, TOL, X, XEND
      INTEGER          I, IFAIL, J
*     .. Local Arrays ..
      DOUBLE PRECISION W(21*N+23), Y(N)
*     .. External Functions ..
      DOUBLE PRECISION X01AAF
      EXTERNAL         X01AAF
*     .. External Subroutines ..
      EXTERNAL         D02CAF, FCN
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D02CAF Example Program Results'
      PI = X01AAF(0.0D0)
      XEND = 8.0D0
      DO 20 J = 4, 5
         TOL = 10.0D0**(-J)
         WRITE (NOUT,*)
         WRITE (NOUT,99999) 'Calculation with TOL =', TOL
         X = 0.0D0
         Y(1) = 0.0D0
         Y(2) = 0.5D0
         Y(3) = PI/5.0D0
         WRITE (NOUT,*) '    X         Y(1)         Y(2)         Y(3)'
         WRITE (NOUT,99998) X, (Y(I),I=1,N)
         IFAIL = 0
*
         CALL D02CAF(X,XEND,N,Y,TOL,FCN,W,IFAIL)
*
         WRITE (NOUT,99998) X, (Y(I),I=1,N)
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
*     .. Intrinsic Functions ..
      INTRINSIC      COS, TAN
*     .. Executable Statements ..
      F(1) = TAN(Y(3))
      F(2) = -0.032D0*TAN(Y(3))/Y(2) - 0.02D0*Y(2)/COS(Y(3))
      F(3) = -0.032D0/Y(2)**2
      RETURN
      END
