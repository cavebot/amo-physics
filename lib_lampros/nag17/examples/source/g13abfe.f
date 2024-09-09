*     G13ABF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NXMAX, NKMAX
      PARAMETER        (NXMAX=50,NKMAX=10)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION STAT, XM, XV
      INTEGER          I, IFAIL, NK, NX
*     .. Local Arrays ..
      DOUBLE PRECISION R(NKMAX), X(NXMAX)
*     .. External Subroutines ..
      EXTERNAL         G13ABF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G13ABF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) NX, NK
      WRITE (NOUT,*)
      IF (NK.GT.0 .AND. NK.LE.NKMAX .AND. NX.GT.0 .AND. NX.LE.NXMAX)
     +    THEN
         READ (NIN,*) (X(I),I=1,NX)
         WRITE (NOUT,99999) 'The first ', NK,
     +     ' coefficients are required'
         IFAIL = 0
*
         CALL G13ABF(X,NX,NK,XM,XV,R,STAT,IFAIL)
*
         WRITE (NOUT,99998) 'The input array has sample mean ', XM
         WRITE (NOUT,99998) 'The input array has sample variance ', XV
         WRITE (NOUT,*) 'The sample autocorrelation coefficients are'
         WRITE (NOUT,*)
         WRITE (NOUT,*) '   Lag    Coeff      Lag    Coeff'
         WRITE (NOUT,99997) (I,R(I),I=1,10)
         WRITE (NOUT,*)
         WRITE (NOUT,99998) 'The value of STAT is ', STAT
      END IF
      STOP
*
99999 FORMAT (1X,A,I2,A)
99998 FORMAT (1X,A,F12.4)
99997 FORMAT (1X,I6,F10.4,I8,F10.4)
      END
