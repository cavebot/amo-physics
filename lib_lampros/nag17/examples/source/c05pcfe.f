*     C05PCF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          N, LDFJAC, LR
      PARAMETER        (N=9,LDFJAC=N,LR=(N*(N+1))/2)
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION FACTOR, FNORM, XTOL
      INTEGER          IFAIL, J, MAXFEV, MODE, NFEV, NJEV, NPRINT
*     .. Local Arrays ..
      DOUBLE PRECISION DIAG(N), FJAC(LDFJAC,N), FVEC(N), QTF(N), R(LR),
     +                 W(N,4), X(N)
*     .. External Functions ..
      DOUBLE PRECISION F06EJF, X02AJF
      EXTERNAL         F06EJF, X02AJF
*     .. External Subroutines ..
      EXTERNAL         C05PCF, FCN
*     .. Intrinsic Functions ..
      INTRINSIC        SQRT
*     .. Executable Statements ..
      WRITE (NOUT,*) 'C05PCF Example Program Results'
      WRITE (NOUT,*)
*     The following starting values provide a rough solution.
      DO 20 J = 1, N
         X(J) = -1.0D0
   20 CONTINUE
      XTOL = SQRT(X02AJF())
      DO 40 J = 1, N
         DIAG(J) = 1.0D0
   40 CONTINUE
      MAXFEV = 1000
      MODE = 2
      FACTOR = 100.0D0
      NPRINT = 0
      IFAIL = 1
*
      CALL C05PCF(FCN,N,X,FVEC,FJAC,LDFJAC,XTOL,MAXFEV,DIAG,MODE,FACTOR,
     +            NPRINT,NFEV,NJEV,R,LR,QTF,W,IFAIL)
*
      IF (IFAIL.EQ.0) THEN
         FNORM = F06EJF(N,FVEC,1)
         WRITE (NOUT,99999) 'Final 2-norm of the residuals =', FNORM
         WRITE (NOUT,*)
         WRITE (NOUT,99998) 'Number of function evaluations =', NFEV
         WRITE (NOUT,*)
         WRITE (NOUT,99998) 'Number of Jacobian evaluations =', NJEV
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Final approximate solution'
         WRITE (NOUT,*)
         WRITE (NOUT,99997) (X(J),J=1,N)
      ELSE
         WRITE (NOUT,99996) 'IFAIL = ', IFAIL
         IF (IFAIL.GT.2) THEN
            WRITE (NOUT,*)
            WRITE (NOUT,*) 'Approximate solution:'
            WRITE (NOUT,*)
            WRITE (NOUT,99997) (X(J),J=1,N)
         END IF
      END IF
      STOP
*
99999 FORMAT (1X,A,D12.4)
99998 FORMAT (1X,A,I10)
99997 FORMAT (1X,3F12.4)
99996 FORMAT (1X,A,I2)
      END
*
      SUBROUTINE FCN(N,X,FVEC,FJAC,LDFJAC,IFLAG)
*     .. Parameters ..
      DOUBLE PRECISION ZERO, ONE, TWO, THREE, FOUR
      PARAMETER      (ZERO=0.0D0,ONE=1.0D0,TWO=2.0D0,THREE=3.0D0,
     +               FOUR=4.0D0)
*     .. Scalar Arguments ..
      INTEGER        IFLAG, LDFJAC, N
*     .. Array Arguments ..
      DOUBLE PRECISION FJAC(LDFJAC,N), FVEC(N), X(N)
*     .. Local Scalars ..
      INTEGER        J, K
*     .. Executable Statements ..
      IF (IFLAG.EQ.0) THEN
*
*        Insert print statements here when NPRINT is positive.
*
         RETURN
      ELSE
         IF (IFLAG.NE.2) THEN
            DO 20 K = 1, N
               FVEC(K) = (THREE-TWO*X(K))*X(K) + ONE
               IF (K.GT.1) FVEC(K) = FVEC(K) - X(K-1)
               IF (K.LT.N) FVEC(K) = FVEC(K) - TWO*X(K+1)
   20       CONTINUE
         ELSE
            DO 60 K = 1, N
               DO 40 J = 1, N
                  FJAC(K,J) = ZERO
   40          CONTINUE
               FJAC(K,K) = THREE - FOUR*X(K)
               IF (K.GT.1) FJAC(K,K-1) = -ONE
               IF (K.LT.N) FJAC(K,K+1) = -TWO
   60       CONTINUE
         END IF
      END IF
      RETURN
      END
