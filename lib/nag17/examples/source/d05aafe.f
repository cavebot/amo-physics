*     D05AAF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          N, NMAX, MN
      PARAMETER        (N=5,NMAX=N,MN=2*N+2)
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Scalars in Common ..
      DOUBLE PRECISION R
*     .. Local Scalars ..
      DOUBLE PRECISION A, ANS, B, LAMBDA, X
      INTEGER          I, IFAIL, IND, IS
*     .. Local Arrays ..
      DOUBLE PRECISION C(NMAX), F(NMAX), W1(NMAX,MN), W2(MN,4), WD(MN)
*     .. External Functions ..
      DOUBLE PRECISION C06DBF, G, K1, K2, X01AAF
      EXTERNAL         C06DBF, G, K1, K2, X01AAF
*     .. External Subroutines ..
      EXTERNAL         D05AAF
*     .. Common blocks ..
      COMMON           R
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D05AAF Example Program Results'
      WRITE (NOUT,*)
      R = X01AAF(0.0D0)
      LAMBDA = 1.0D0
      A = 0.0D0
      B = 1.0D0
      IND = 2
      IFAIL = 0
      WRITE (NOUT,*)
     +'Kernel is centro-symmetric and G is even so the solution is even'
      WRITE (NOUT,*)
*
      CALL D05AAF(LAMBDA,A,B,K1,K2,G,F,C,N,IND,W1,W2,WD,NMAX,MN,IFAIL)
*
      WRITE (NOUT,*) 'Chebyshev coefficients'
      WRITE (NOUT,*)
      WRITE (NOUT,99998) (C(I),I=1,N)
      WRITE (NOUT,*)
      X = 0.1D0
*     Note that X has to be transformed to range [-1,1]
      IS = 1
      IF (IND.EQ.1) THEN
         IS = 3
      ELSE
         IF (IND.EQ.2) IS = 2
      END IF
      ANS = C06DBF(2.0D0/(B-A)*(X-0.5D0*(B+A)),C,N,IS)
      WRITE (NOUT,99999) 'X=', X, '    ANS=', ANS
      STOP
*
99999 FORMAT (1X,A,F5.2,A,1F10.4)
99998 FORMAT (1X,5D14.4)
      END
*
      DOUBLE PRECISION FUNCTION K1(X,S)
*     .. Scalar Arguments ..
      DOUBLE PRECISION             S, X
*     .. Executable Statements ..
      K1 = S*(1.0D0-X)
      RETURN
      END
*
      DOUBLE PRECISION FUNCTION K2(X,S)
*     .. Scalar Arguments ..
      DOUBLE PRECISION             S, X
*     .. Executable Statements ..
      K2 = X*(1.0D0-S)
      RETURN
      END
*
      DOUBLE PRECISION FUNCTION G(X)
*     .. Scalar Arguments ..
      DOUBLE PRECISION            X
*     .. Scalars in Common ..
      DOUBLE PRECISION            R
*     .. Intrinsic Functions ..
      INTRINSIC                   SIN
*     .. Common blocks ..
      COMMON                      R
*     .. Executable Statements ..
      G = SIN(R*X)*(1.0D0-1.0D0/(R*R))
      RETURN
      END
