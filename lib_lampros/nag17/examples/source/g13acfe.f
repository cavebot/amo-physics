*     G13ACF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NLMAX, NKMAX
      PARAMETER        (NLMAX=5,NKMAX=10)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, NK, NL, NVL
*     .. Local Arrays ..
      DOUBLE PRECISION AR(NLMAX), P(NLMAX), R(NKMAX), V(NLMAX)
*     .. External Subroutines ..
      EXTERNAL         G13ACF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G13ACF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) NK, NL
      WRITE (NOUT,*)
      IF (NL.GT.0 .AND. NL.LE.NLMAX .AND. NK.GT.0 .AND. NK.LE.NKMAX)
     +    THEN
         READ (NIN,*) (R(I),I=1,NK)
         IFAIL = 1
*
         CALL G13ACF(R,NK,NL,P,V,AR,NVL,IFAIL)
*
         IF (IFAIL.NE.0) THEN
            WRITE (NOUT,99999) 'G13ACF fails. IFAIL = ', IFAIL
            WRITE (NOUT,*)
         END IF
         IF (IFAIL.EQ.3) THEN
            WRITE (NOUT,99998) '   Only', NVL,
     +        'valid sets were generated'
            WRITE (NOUT,*)
         END IF
         IF (IFAIL.EQ.0 .OR. IFAIL.EQ.3) THEN
            WRITE (NOUT,*)
     +        'Lag  Partial    Predictor error  Autoregressive'
            WRITE (NOUT,*) '     autocorrn  variance ratio   parameter'
            WRITE (NOUT,*)
            WRITE (NOUT,99997) (I,P(I),V(I),AR(I),I=1,NVL)
         END IF
      END IF
      STOP
*
99999 FORMAT (1X,A,I1)
99998 FORMAT (1X,A,I2,A)
99997 FORMAT (1X,I2,F9.3,F16.3,F14.3)
      END
