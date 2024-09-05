*     E01RAF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          N
      PARAMETER        (N=5)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, M
*     .. Local Arrays ..
      DOUBLE PRECISION A(N), F(N), U(N), X(N)
      INTEGER          IW(N)
*     .. External Subroutines ..
      EXTERNAL         E01RAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'E01RAF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) (X(I),I=1,N)
      READ (NIN,*) (F(I),I=1,N)
      IFAIL = 0
*
      CALL E01RAF(N,X,F,M,A,U,IW,IFAIL)
*
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'The values of U(J) are'
      WRITE (NOUT,99999) (U(I),I=1,M-1)
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'The Thiele coefficients A(J) are'
      WRITE (NOUT,99999) (A(I),I=1,M)
      STOP
*
99999 FORMAT (1X,1P,4D12.4,/)
      END
