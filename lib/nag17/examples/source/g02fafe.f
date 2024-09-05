*     G02FAF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX
      PARAMETER        (NMAX=24)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION RMS
      INTEGER          I, IFAIL, IP, J, N, NRES
*     .. Local Arrays ..
      DOUBLE PRECISION H(NMAX), RES(NMAX), SRES(NMAX,4)
*     .. External Subroutines ..
      EXTERNAL         G02FAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G02FAF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, IP, NRES, RMS
      IF (NRES.LT.NMAX) THEN
         DO 20 I = 1, NRES
            READ (NIN,*) RES(I), H(I)
   20    CONTINUE
         IFAIL = 0
*
         CALL G02FAF(N,IP,NRES,RES,H,RMS,SRES,NMAX,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) '        Internally    Internally'
         WRITE (NOUT,*)
     +   'Obs.   standardized  standardized   Cook''s D   Atkinson''s T'
         WRITE (NOUT,*) '         residuals     residuals'
         WRITE (NOUT,*)
         DO 40 I = 1, NRES
            WRITE (NOUT,99999) I, (SRES(I,J),J=1,4)
   40    CONTINUE
      END IF
      STOP
*
99999 FORMAT (1X,I2,4F13.3)
      END
