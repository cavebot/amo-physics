*     G12AAF Example Program Text
*     Mark 15 Release. NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX
      PARAMETER        (NMAX=18)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, N, ND
*     .. Local Arrays ..
      DOUBLE PRECISION P(NMAX), PSIG(NMAX), T(NMAX), TP(NMAX)
      INTEGER          IC(NMAX), IFREQ(NMAX), IWK(NMAX)
*     .. External Subroutines ..
      EXTERNAL         G12AAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G12AAF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      IF (N.LE.NMAX) THEN
         READ (NIN,*) (T(I),IC(I),IFREQ(I),I=1,N)
         IFAIL = 0
*
         CALL G12AAF(N,T,IC,'Frequencies',IFREQ,ND,TP,P,PSIG,IWK,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) '  Time   Survival    Standard'
         WRITE (NOUT,*) '        probability  deviation'
         WRITE (NOUT,*)
         DO 20 I = 1, ND
            WRITE (NOUT,99999) TP(I), P(I), PSIG(I)
   20    CONTINUE
      END IF
      STOP
*
99999 FORMAT (1X,F6.1,F10.3,2X,F10.3)
      END
