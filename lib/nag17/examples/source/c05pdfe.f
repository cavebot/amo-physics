*     C05PDF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          N, LDFJAC, LR
      PARAMETER        (N=9,LDFJAC=N,LR=(N*(N+1))/2)
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      DOUBLE PRECISION ZERO, ONE, TWO, THREE, FOUR
      PARAMETER        (ZERO=0.0D0,ONE=1.0D0,TWO=2.0D0,THREE=3.0D0,
     +                 FOUR=4.0D0)
*     .. Local Scalars ..
      DOUBLE PRECISION FACTOR, FNORM, XTOL
      INTEGER          ICOUNT, IFAIL, IREVCM, J, K, MODE
*     .. Local Arrays ..
      DOUBLE PRECISION DIAG(N), FJAC(LDFJAC,N), FVEC(N), QTF(N), R(LR),
     +                 W(N,4), X(N)
*     .. External Functions ..
      DOUBLE PRECISION F06EJF, X02AJF
      EXTERNAL         F06EJF, X02AJF
*     .. External Subroutines ..
      EXTERNAL         C05PDF
*     .. Intrinsic Functions ..
      INTRINSIC        SQRT
*     .. Executable Statements ..
      WRITE (NOUT,*) 'C05PDF Example Program Results'
*     The following starting values provide a rough solution.
      DO 20 J = 1, N
         X(J) = -1.0D0
   20 CONTINUE
      XTOL = SQRT(X02AJF())
      DO 40 J = 1, N
         DIAG(J) = 1.0D0
   40 CONTINUE
      MODE = 2
      FACTOR = 100.0D0
      ICOUNT = 0
      IFAIL = 1
      IREVCM = 0
*
   60 CALL C05PDF(IREVCM,N,X,FVEC,FJAC,LDFJAC,XTOL,DIAG,MODE,FACTOR,R,
     +            LR,QTF,W,IFAIL)
*
      IF (IREVCM.EQ.1) THEN
         ICOUNT = ICOUNT + 1
*        Insert print statements here to monitor progess if desired
         GO TO 60
      ELSE IF (IREVCM.EQ.2) THEN
*        Evaluate functions at current point
         DO 80 K = 1, N
            FVEC(K) = (THREE-TWO*X(K))*X(K) + ONE
            IF (K.GT.1) FVEC(K) = FVEC(K) - X(K-1)
            IF (K.LT.N) FVEC(K) = FVEC(K) - TWO*X(K+1)
   80    CONTINUE
         GO TO 60
      ELSE IF (IREVCM.EQ.3) THEN
*        Evaluate Jacobian at current point
         DO 120 K = 1, N
            DO 100 J = 1, N
               FJAC(K,J) = ZERO
  100       CONTINUE
            FJAC(K,K) = THREE - FOUR*X(K)
            IF (K.NE.1) FJAC(K,K-1) = -ONE
            IF (K.NE.N) FJAC(K,K+1) = -TWO
  120    CONTINUE
         GO TO 60
      END IF
*
      WRITE (NOUT,*)
      IF (IFAIL.EQ.0) THEN
         FNORM = F06EJF(N,FVEC,1)
         WRITE (NOUT,99999) 'Final 2 norm of the residuals after',
     +     ICOUNT, ' iterations is ', FNORM
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Final approximate solution'
         WRITE (NOUT,99998) (X(J),J=1,N)
      ELSE
         WRITE (NOUT,99999) 'IFAIL =', IFAIL
         IF (IFAIL.GT.2) THEN
            WRITE (NOUT,*) 'Approximate solution'
            WRITE (NOUT,99998) (X(J),J=1,N)
         END IF
      END IF
      STOP
*
99999 FORMAT (1X,A,I4,A,D12.4)
99998 FORMAT (5X,3F12.4)
      END
