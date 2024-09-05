*     G01DCF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          N, IW
      PARAMETER        (N=6,IW=3*N/2)
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION ERREST, ETOL, EXP1, EXP2, SUMSSQ
      INTEGER          I, IFAIL, J, K
*     .. Local Arrays ..
      DOUBLE PRECISION PP(N), VEC(N*(N+1)/2), WORK(IW)
*     .. External Subroutines ..
      EXTERNAL         G01DAF, G01DCF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G01DCF Example Program Results'
      ETOL = 0.0001D0
      IFAIL = 0
*
      CALL G01DAF(N,PP,ETOL,ERREST,WORK,IW,IFAIL)
*
      EXP1 = PP(N)
      EXP2 = PP(N-1)
      SUMSSQ = 0.0D0
      DO 20 I = 1, N
         SUMSSQ = SUMSSQ + PP(I)*PP(I)
   20 CONTINUE
      IFAIL = 0
*
      CALL G01DCF(N,EXP1,EXP2,SUMSSQ,VEC,IFAIL)
*
      WRITE (NOUT,*)
      WRITE (NOUT,99999) 'Sample size = ', N
      WRITE (NOUT,*) 'Variance-covariance matrix'
      K = 1
      DO 40 J = 1, N
         WRITE (NOUT,99998) (VEC(I),I=K,K+J-1)
         K = K + J
   40 CONTINUE
      STOP
*
99999 FORMAT (1X,A,I2)
99998 FORMAT (1X,6F8.4)
      END
