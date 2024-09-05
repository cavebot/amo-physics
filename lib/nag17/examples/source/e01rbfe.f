*     E01RBF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          M
      PARAMETER        (M=4)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION F, X
      INTEGER          I, IFAIL
*     .. Local Arrays ..
      DOUBLE PRECISION A(M), U(M)
*     .. External Subroutines ..
      EXTERNAL         E01RBF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'E01RBF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) (A(I),I=1,M)
      READ (NIN,*) (U(I),I=1,M-1)
      READ (NIN,*) X
      WRITE (NOUT,*)
      WRITE (NOUT,99999) 'X =', X
      IFAIL = 0
*
      CALL E01RBF(M,A,U,X,F,IFAIL)
*
      WRITE (NOUT,*)
      WRITE (NOUT,99999) 'The value of R(X) is ', F
      STOP
*
99999 FORMAT (1X,A,1P,D12.4)
      END
