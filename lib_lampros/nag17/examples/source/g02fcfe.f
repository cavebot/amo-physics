*     G02FCF Example Program Text.
*     Mark 15 Release. NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          N
      PARAMETER        (N=10)
*     .. Local Scalars ..
      DOUBLE PRECISION D, PDL, PDU
      INTEGER          I, IFAIL, IP
*     .. Local Arrays ..
      DOUBLE PRECISION RES(N), WORK(N)
*     .. External Subroutines ..
      EXTERNAL         G02FCF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G02FCF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) IP
      READ (NIN,*) (RES(I),I=1,N)
*
      IFAIL = 0
*
      CALL G02FCF(N,IP,RES,D,PDL,PDU,WORK,IFAIL)
*
      WRITE (NOUT,*)
      WRITE (NOUT,99999) ' Durbin-Watson statistic ', D
      WRITE (NOUT,*)
      WRITE (NOUT,99998) ' Lower and upper bound ', PDL, PDU
      STOP
*
99999 FORMAT (1X,A,F10.4)
99998 FORMAT (1X,A,2F10.4)
      END
