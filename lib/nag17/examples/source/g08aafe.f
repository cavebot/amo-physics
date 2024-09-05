*     G08AAF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          N
      PARAMETER        (N=17)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION SIG
      INTEGER          IFAIL, IS, N1
*     .. Local Arrays ..
      DOUBLE PRECISION X(N), Y(N)
*     .. External Subroutines ..
      EXTERNAL         G08AAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G08AAF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) X, Y
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Sign test'
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Data values'
      WRITE (NOUT,*)
      WRITE (NOUT,99999) X, Y
      IFAIL = 0
*
      CALL G08AAF(X,Y,N,IS,N1,SIG,IFAIL)
*
      WRITE (NOUT,*)
      WRITE (NOUT,99998) 'Test statistic   ', IS
      WRITE (NOUT,99998) 'Observations     ', N1
      WRITE (NOUT,99997) 'Lower tail prob. ', SIG
      STOP
*
99999 FORMAT (4X,17F3.0)
99998 FORMAT (1X,A,I5)
99997 FORMAT (1X,A,F5.3)
      END
