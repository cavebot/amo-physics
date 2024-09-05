*     G08AEF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          K, N, IX
      PARAMETER        (K=3,N=18,IX=K)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION FR, SIG
      INTEGER          I, IFAIL, J
*     .. Local Arrays ..
      DOUBLE PRECISION W1(K), W2(K), X(IX,N)
*     .. External Subroutines ..
      EXTERNAL         G08AEF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G08AEF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) ((X(I,J),J=1,18),I=1,3)
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Friedman test'
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Data values'
      WRITE (NOUT,*)
      WRITE (NOUT,*) '   Group Group Group'
      WRITE (NOUT,*) '     1     2     3'
      WRITE (NOUT,99997) ((X(I,J),I=1,3),J=1,18)
      IFAIL = 0
*
      CALL G08AEF(X,IX,K,N,W1,W2,FR,SIG,IFAIL)
*
      WRITE (NOUT,*)
      WRITE (NOUT,99999) 'Test statistic       ', FR
      WRITE (NOUT,99998) 'Degrees of freedom   ', K - 1
      WRITE (NOUT,99999) 'Significance         ', SIG
      STOP
*
99999 FORMAT (1X,A,F6.3)
99998 FORMAT (1X,A,I6)
99997 FORMAT (1X,F7.1,2F6.1)
      END
