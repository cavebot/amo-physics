*     D02CHF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          N
      PARAMETER        (N=3)
*     .. Local Scalars ..
      DOUBLE PRECISION HMAX, PI, TOL, X, XEND
      INTEGER          I, IFAIL, IRELAB, J
*     .. Local Arrays ..
      DOUBLE PRECISION W(N*21+28), Y(N)
*     .. External Functions ..
      DOUBLE PRECISION G, X01AAF
      EXTERNAL         G, X01AAF
*     .. External Subroutines ..
      EXTERNAL         D02CHF, FCN
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D02CHF Example Program Results'
      XEND = 10.0D0
      HMAX = 0.0D0
      IRELAB = 0
      PI = X01AAF(0.0D0)
      DO 20 J = 4, 5
         TOL = 10.0D0**(-J)
         WRITE (NOUT,*)
         WRITE (NOUT,99997) 'Calculation with TOL =', TOL
         X = 0.0D0
         Y(1) = 0.5D0
         Y(2) = 0.5D0
         Y(3) = PI/5.0D0
         IFAIL = 0
*
         CALL D02CHF(X,XEND,N,Y,TOL,IRELAB,HMAX,FCN,G,W,IFAIL)
*
         WRITE (NOUT,99999) ' Root of Y(1) at', X
         WRITE (NOUT,99998) ' Solution is', (Y(I),I=1,N)
   20 CONTINUE
      STOP
*
99999 FORMAT (1X,A,F7.4)
99998 FORMAT (1X,A,3F13.5)
99997 FORMAT (1X,A,D8.1)
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
*
      DOUBLE PRECISION FUNCTION G(T,Y)
*     .. Parameters ..
      INTEGER                     N
      PARAMETER                   (N=3)
*     .. Scalar Arguments ..
      DOUBLE PRECISION            T
*     .. Array Arguments ..
      DOUBLE PRECISION            Y(N)
*     .. Executable Statements ..
      G = Y(1)
      RETURN
      END
