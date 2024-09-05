*     G01CEF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION X
      INTEGER          I, IFAIL
*     .. Local Arrays ..
      DOUBLE PRECISION P(7)
*     .. External Functions ..
      DOUBLE PRECISION G01CEF
      EXTERNAL         G01CEF
*     .. Data statements ..
      DATA             P/0.95D0, 0.5D0, 0.0D0, 0.995D0, 0.75D0, 2.0D0,
     +                 0.001D0/
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G01CEF Example Program Results'
      WRITE (NOUT,*)
      WRITE (NOUT,*) '  Prob.   Deviate'
      WRITE (NOUT,*)
      DO 20 I = 1, 7
         IFAIL = 1
*
         X = G01CEF(P(I),IFAIL)
*
         IF (IFAIL.NE.0) THEN
            WRITE (NOUT,99999) P(I), '     Failed in G01CEF. IFAIL =',
     +        IFAIL
         ELSE
            WRITE (NOUT,99998) P(I), X
         END IF
   20 CONTINUE
      STOP
*
99999 FORMAT (1X,F7.3,A,I2)
99998 FORMAT (1X,F7.3,F11.4)
      END
