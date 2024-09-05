*     G13AAF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NXMAX
      PARAMETER        (NXMAX=20)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, ND, NDS, NS, NX, NXD
*     .. Local Arrays ..
      DOUBLE PRECISION X(NXMAX), XD(NXMAX)
*     .. External Subroutines ..
      EXTERNAL         G13AAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G13AAF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) NX, ND, NDS, NS
      IF (NX.GT.0 .AND. NX.LE.NXMAX) THEN
         READ (NIN,*) (X(I),I=1,NX)
         WRITE (NOUT,*)
         WRITE (NOUT,99999) 'Non-seasonal differencing of order ', ND,
     +     ' and seasonal differencing'
         WRITE (NOUT,99999) 'of order ', NDS, ' with seasonality ', NS,
     +     ' are applied'
         IFAIL = 0
*
         CALL G13AAF(X,NX,ND,NDS,NS,XD,NXD,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,99998) 'The output array holds ', NX,
     +     ' values, of which the first ', NXD,
     +     ' are differenced values'
         WRITE (NOUT,*)
         WRITE (NOUT,99997) (XD(I),I=1,NX)
      END IF
      STOP
*
99999 FORMAT (1X,A,I1,A,I1,A)
99998 FORMAT (1X,A,I2,A,I2,A)
99997 FORMAT (1X,5F9.1)
      END
