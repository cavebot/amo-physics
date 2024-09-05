*     C05NBF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          N, LWA
      PARAMETER        (N=9,LWA=(N*(3*N+13))/2)
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION FNORM, TOL
      INTEGER          I, IFAIL, J
*     .. Local Arrays ..
      DOUBLE PRECISION FVEC(N), WA(LWA), X(N)
*     .. External Functions ..
      DOUBLE PRECISION F06EJF, X02AJF
      EXTERNAL         F06EJF, X02AJF
*     .. External Subroutines ..
      EXTERNAL         C05NBF, FCN
*     .. Intrinsic Functions ..
      INTRINSIC        SQRT
*     .. Executable Statements ..
      WRITE (NOUT,*) 'C05NBF Example Program Results'
      WRITE (NOUT,*)
*     The following starting values provide a rough solution.
      DO 20 J = 1, N
         X(J) = -1.0D0
   20 CONTINUE
      TOL = SQRT(X02AJF())
      IFAIL = 1
*
      CALL C05NBF(FCN,N,X,FVEC,TOL,WA,LWA,IFAIL)
*
      IF (IFAIL.EQ.0) THEN
         FNORM = F06EJF(N,FVEC,1)
         WRITE (NOUT,99999) 'Final 2-norm of the residuals =', FNORM
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Final approximate solution'
         WRITE (NOUT,*)
         WRITE (NOUT,99998) (X(J),J=1,N)
      ELSE
         WRITE (NOUT,99997) 'IFAIL = ', IFAIL
         IF (IFAIL.GT.1) THEN
            WRITE (NOUT,*)
            WRITE (NOUT,*) 'Approximate solution'
            WRITE (NOUT,*)
            WRITE (NOUT,99998) (X(I),I=1,N)
         END IF
      END IF
      STOP
*
99999 FORMAT (1X,A,D12.4)
99998 FORMAT (1X,3F12.4)
99997 FORMAT (1X,A,I2)
      END
*
      SUBROUTINE FCN(N,X,FVEC,IFLAG)
*     .. Parameters ..
      DOUBLE PRECISION ONE, TWO, THREE
      PARAMETER      (ONE=1.0D0,TWO=2.0D0,THREE=3.0D0)
*     .. Scalar Arguments ..
      INTEGER        IFLAG, N
*     .. Array Arguments ..
      DOUBLE PRECISION FVEC(N), X(N)
*     .. Local Scalars ..
      INTEGER        K
*     .. Executable Statements ..
      DO 20 K = 1, N
         FVEC(K) = (THREE-TWO*X(K))*X(K) + ONE
         IF (K.GT.1) FVEC(K) = FVEC(K) - X(K-1)
         IF (K.LT.N) FVEC(K) = FVEC(K) - TWO*X(K+1)
   20 CONTINUE
      RETURN
      END
