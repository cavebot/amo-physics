*     G08BAF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          N
      PARAMETER        (N=12)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION PV, PW, V, W
      INTEGER          I, IFAIL, ITEST, N1
*     .. Local Arrays ..
      DOUBLE PRECISION WK(N), X(N)
*     .. External Subroutines ..
      EXTERNAL         G08BAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G08BAF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) X
      N1 = 6
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Mood''s test and David''s test'
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Data values'
      WRITE (NOUT,*)
      WRITE (NOUT,99999) '    Group 1  ', (X(I),I=1,N1)
      WRITE (NOUT,*)
      WRITE (NOUT,99999) '    Group 2  ', (X(I),I=N1+1,N)
      ITEST = 0
      IFAIL = 0
*
      CALL G08BAF(X,N,N1,WK,ITEST,W,V,PW,PV,IFAIL)
*
      WRITE (NOUT,99998) '    Mood''s measure  = ', W,
     +  '   Significance = ', PW
      WRITE (NOUT,99998) '    David''s measure = ', V,
     +  '   Significance = ', PV
      STOP
*
99999 FORMAT (1X,A,8F4.0,/(13X,8F4.0))
99998 FORMAT (1X,A,F8.3,A,F8.4)
      END
