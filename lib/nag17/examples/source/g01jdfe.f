*     G01JDF Example Program Text
*     Mark 15 Release. NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          N
      PARAMETER        (N=10)
*     .. Local Scalars ..
      DOUBLE PRECISION C, D, PROB
      INTEGER          I, IFAIL
      CHARACTER        METHOD
*     .. Local Arrays ..
      DOUBLE PRECISION RLAM(N), WORK(N+1)
*     .. External Subroutines ..
      EXTERNAL         G01JDF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G01JDF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) METHOD, D, C
      READ (NIN,*) (RLAM(I),I=1,N)
*
      IFAIL = 0
*
      CALL G01JDF(METHOD,N,RLAM,D,C,PROB,WORK,IFAIL)
*
      WRITE (NOUT,*)
      WRITE (NOUT,99999) ' Values of lambda ', (RLAM(I),I=1,N)
      WRITE (NOUT,99999) ' Value of D       ', D
      WRITE (NOUT,99999) ' value of C       ', C
      WRITE (NOUT,*)
      WRITE (NOUT,99998) ' Probability = ', PROB
      STOP
*
99999 FORMAT (1X,A,10F6.2)
99998 FORMAT (1X,A,F10.4)
      END
