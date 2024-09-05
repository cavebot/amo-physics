*     G01EPF Example Program Text
*     Mark 15 Release. NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX
      PARAMETER        (NMAX=10)
*     .. Local Scalars ..
      DOUBLE PRECISION D, PDL, PDU
      INTEGER          IFAIL, IP, N
*     .. Local Arrays ..
      DOUBLE PRECISION WORK(NMAX)
*     .. External Subroutines ..
      EXTERNAL         G01EPF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G01EPF Example Program Results '
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, IP, D
*
      IF (N.LE.NMAX) THEN
         IFAIL = 0
*
         CALL G01EPF(N,IP,D,PDL,PDU,WORK,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,99999) ' Durbin-Watson statistic ', D
         WRITE (NOUT,*)
         WRITE (NOUT,99998) ' Probability for the lower bound = ',
     +     PDL
         WRITE (NOUT,99998) ' Probability for the upper bound = ',
     +     PDU
      ELSE
         WRITE (NOUT,*) ' N is larger than NMAX'
      END IF
      STOP
*
99999 FORMAT (1X,A,F10.4)
99998 FORMAT (1X,A,F10.4)
      END
