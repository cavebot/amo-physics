*     G01ALF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX
      PARAMETER        (NMAX=12)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, N
*     .. Local Arrays ..
      DOUBLE PRECISION RES(5), X(NMAX)
      INTEGER          IWRK(NMAX)
*     .. External Subroutines ..
      EXTERNAL         G01ALF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G01ALF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, (X(I),I=1,N)
      IFAIL = 0
*
      CALL G01ALF(N,X,IWRK,RES,IFAIL)
*
      WRITE (NOUT,*)
      WRITE (NOUT,99999) 'Maximum     ', RES(5)
      WRITE (NOUT,99999) 'Upper Hinge ', RES(4)
      WRITE (NOUT,99999) 'Median      ', RES(3)
      WRITE (NOUT,99999) 'Lower Hinge ', RES(2)
      WRITE (NOUT,99999) 'Minimum     ', RES(1)
      STOP
*
99999 FORMAT (1X,A,F16.4)
      END
