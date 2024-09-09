*     G08ACF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          N
      PARAMETER        (N=39)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION P
      INTEGER          I, I1, I2, IFAIL, N1
*     .. Local Arrays ..
      DOUBLE PRECISION W1(N), X(N)
*     .. External Subroutines ..
      EXTERNAL         G08ACF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G08ACF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) (X(I),I=1,N)
      N1 = 16
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Median test'
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Data values'
      WRITE (NOUT,*)
      WRITE (NOUT,99999) '    Group 1  ', (X(I),I=1,N1)
      WRITE (NOUT,*)
      WRITE (NOUT,99999) '    Group 2  ', (X(I),I=N1+1,N)
      IFAIL = 0
*
      CALL G08ACF(X,N,N1,W1,I1,I2,P,IFAIL)
*
      WRITE (NOUT,*)
      WRITE (NOUT,99998) I1, ' scores below median in group 1'
      WRITE (NOUT,99998) I2, ' scores below median in group 2'
      WRITE (NOUT,*)
      WRITE (NOUT,99997) '     Significance  ', P
      STOP
*
99999 FORMAT (1X,A,8F4.0,/(14X,8F4.0))
99998 FORMAT (1X,I6,A)
99997 FORMAT (1X,A,F8.5)
      END
