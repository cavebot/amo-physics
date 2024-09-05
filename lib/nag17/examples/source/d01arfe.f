*     D01ARF Example Program Text
*     Mark 16 Revised. NAG Copyright 1993.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          MAXRUL
      PARAMETER        (MAXRUL=0)
*     .. Local Scalars ..
      DOUBLE PRECISION A, ABSACC, ACC, ANS, B, RELACC
      INTEGER          IFAIL, IPARM, N
*     .. Local Arrays ..
      DOUBLE PRECISION ALPHA(390)
*     .. External Functions ..
      DOUBLE PRECISION F1, F2
      EXTERNAL         F1, F2
*     .. External Subroutines ..
      EXTERNAL         D01ARF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D01ARF Example Program Results'
      RELACC = 0.0D0
      ABSACC = 1.0D-5
*     Definite integral of F1(x) - no expansion
      IPARM = 0
      IFAIL = 1
      A = 0.0D0
      B = 1.0D0
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Definite integral of 4/(1+x*x) over (0,1)'
*
      CALL D01ARF(A,B,F1,RELACC,ABSACC,MAXRUL,IPARM,ACC,ANS,N,ALPHA,
     +            IFAIL)
*
      IF (IFAIL.NE.0) WRITE (NOUT,99997) 'D01ARF fails. IFAIL =',
     +    IFAIL
      IF (IFAIL.LE.1) THEN
         WRITE (NOUT,99999) 'Estimated value of the integral =', ANS
         WRITE (NOUT,99998) 'Estimated absolute error =', ACC
         WRITE (NOUT,99997) 'Number of points used =', N
      END IF
*     Definite integral of F2(x) - with expansion
      IPARM = 1
      IFAIL = 1
      A = 1.0D0
      B = 2.0D0
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Definite integral of x**(1/8) over (1,2)'
*
      CALL D01ARF(A,B,F2,RELACC,ABSACC,MAXRUL,IPARM,ACC,ANS,N,ALPHA,
     +            IFAIL)
*
      IF (IFAIL.NE.0) WRITE (NOUT,99997) 'D01ARF fails. IFAIL =',
     +    IFAIL
      IF (IFAIL.LE.1) THEN
         WRITE (NOUT,99999) 'Estimated value of the integral =', ANS
         WRITE (NOUT,99998) 'Estimated absolute error =', ACC
         WRITE (NOUT,99997) 'Number of points used =', N
      END IF
*     Indefinite integral of F2(x)
      IPARM = 2
      IFAIL = 0
      A = 1.2D0
      B = 1.8D0
      WRITE (NOUT,*)
      WRITE (NOUT,*)
     +  'Indefinite integral of x**(1/8) over (1.2,1.8)'
*
      CALL D01ARF(A,B,F2,RELACC,ABSACC,MAXRUL,IPARM,ACC,ANS,N,ALPHA,
     +            IFAIL)
*
      WRITE (NOUT,99999) 'Estimated value of the integral =', ANS
      STOP
*
99999 FORMAT (1X,A,F9.5)
99998 FORMAT (1X,A,D10.2)
99997 FORMAT (1X,A,I4)
      END
*
      DOUBLE PRECISION FUNCTION F1(X)
*     .. Scalar Arguments ..
      DOUBLE PRECISION             X
*     .. Executable Statements ..
      F1 = 4.0D0/(1.0D0+X*X)
      RETURN
      END
*
      DOUBLE PRECISION FUNCTION F2(X)
*     .. Scalar Arguments ..
      DOUBLE PRECISION             X
*     .. Executable Statements ..
      F2 = X**0.125D0
      RETURN
      END
