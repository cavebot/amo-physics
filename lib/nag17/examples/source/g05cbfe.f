*     G05CBF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION X
      INTEGER          I
*     .. External Functions ..
      DOUBLE PRECISION G05CAF
      EXTERNAL         G05CAF
*     .. External Subroutines ..
      EXTERNAL         G05CBF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G05CBF Example Program Results'
      WRITE (NOUT,*)
*
      CALL G05CBF(0)
*
      DO 20 I = 1, 5
         X = G05CAF(X)
         WRITE (NOUT,99999) X
   20 CONTINUE
      STOP
*
99999 FORMAT (1X,F10.4)
      END
