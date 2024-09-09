*     D02YAF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          N, IW
      PARAMETER        (N=3,IW=7)
*     .. Local Scalars ..
      DOUBLE PRECISION H, X
      INTEGER          I
*     .. Local Arrays ..
      DOUBLE PRECISION W(N,IW), Y(N)
*     .. External Functions ..
      DOUBLE PRECISION X01AAF
      EXTERNAL         X01AAF
*     .. External Subroutines ..
      EXTERNAL         D02YAF, FCN
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D02YAF Example Program Results'
      X = 0.0D0
      H = 0.5D0
      Y(1) = 0.0D0
      Y(2) = 0.5D0
      Y(3) = X01AAF(X)/6.0D0
      WRITE (NOUT,*)
      WRITE (NOUT,*)
     +  '   X      Y      E(1)       V      E(2)      PHI     E(3)'
      WRITE (NOUT,99999) X, Y(1), Y(2), Y(3)
      DO 20 I = 1, 5
         CALL FCN(X,Y,W(1,1))
*
         CALL D02YAF(X,H,N,Y,FCN,W,N,IW)
*
         X = X + H
         WRITE (NOUT,99998) X, Y(1), W(1,6), Y(2), W(2,6), Y(3), W(3,6)
   20 CONTINUE
      STOP
*
99999 FORMAT (F7.1,3(F8.4,10X))
99998 FORMAT (1X,F6.1,3(F8.4,D10.2))
      END
*
      SUBROUTINE FCN(T,Z,F)
*     .. Parameters ..
      INTEGER        N
      PARAMETER      (N=3)
*     .. Scalar Arguments ..
      DOUBLE PRECISION T
*     .. Array Arguments ..
      DOUBLE PRECISION F(N), Z(N)
*     .. Intrinsic Functions ..
      INTRINSIC      COS, TAN
*     .. Executable Statements ..
      F(1) = TAN(Z(3))
      F(2) = -0.032D0*TAN(Z(3))/Z(2) - 0.02D0*Z(2)/COS(Z(3))
      F(3) = -0.032D0/Z(2)**2
      RETURN
      END
