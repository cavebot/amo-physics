*     D02CGF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          N, M
      PARAMETER        (N=3,M=1)
*     .. Local Scalars ..
      DOUBLE PRECISION HMAX, PI, TOL, VAL, X, XEND
      INTEGER          I, IFAIL
*     .. Local Arrays ..
      DOUBLE PRECISION W(21*N+28), Y(N)
*     .. External Functions ..
      DOUBLE PRECISION X01AAF
      EXTERNAL         X01AAF
*     .. External Subroutines ..
      EXTERNAL         D02CGF, FCN
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D02CGF Example Program Results'
      XEND = 10.0D0
      HMAX = 0.0D0
      VAL = 0.0D0
      PI = X01AAF(0.0D0)
      DO 20 I = 4, 5
         TOL = 10.0D0**(-I)
         WRITE (NOUT,*)
         WRITE (NOUT,99998) 'Calculation with TOL =', TOL
         X = 0.0D0
         Y(1) = 0.5D0
         Y(2) = 0.5D0
         Y(3) = PI/5.0D0
         IFAIL = 0
*
         CALL D02CGF(X,XEND,N,Y,TOL,HMAX,M,VAL,FCN,W,IFAIL)
*
         WRITE (NOUT,99999) ' Y(M) changes sign at X = ', X
   20 CONTINUE
      STOP
*
99999 FORMAT (1X,A,F7.4)
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
*     .. Intrinsic Functions ..
      INTRINSIC      COS, TAN
*     .. Executable Statements ..
      F(1) = TAN(Y(3))
      F(2) = -0.032D0*TAN(Y(3))/Y(2) - 0.02D0*Y(2)/COS(Y(3))
      F(3) = -0.032D0/Y(2)**2
      RETURN
      END
