*     G08DAF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          N, K, IX
      PARAMETER        (N=10,K=3,IX=K)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION P, W
      INTEGER          I, IFAIL, J
*     .. Local Arrays ..
      DOUBLE PRECISION RNK(IX,N), X(IX,N)
*     .. External Subroutines ..
      EXTERNAL         G08DAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G08DAF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) ((X(I,J),J=1,N),I=1,K)
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Kendall''s coefficient of concordance'
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Data values'
      WRITE (NOUT,*)
      WRITE (NOUT,99999) ('Comparison ',I,' scores  ',(X(I,J),J=1,N),
     +  I=1,K)
      IFAIL = 0
*
      CALL G08DAF(X,IX,K,N,RNK,W,P,IFAIL)
*
      WRITE (NOUT,*)
      WRITE (NOUT,99998) 'Kendall''s coefficient =', W
      WRITE (NOUT,99998) '          Significance =', P
      STOP
*
99999 FORMAT (1X,A,I1,A,10F5.1)
99998 FORMAT (1X,A,F8.3)
      END
