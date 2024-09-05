*     G05DZF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      INTEGER          I
      LOGICAL          X
*     .. External Functions ..
      LOGICAL          G05DZF
      EXTERNAL         G05DZF
*     .. External Subroutines ..
      EXTERNAL         G05CBF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G05DZF Example Program Results'
      WRITE (NOUT,*)
      CALL G05CBF(0)
      DO 20 I = 1, 5
*
         X = G05DZF(0.6D0)
*
         WRITE (NOUT,99999) X
   20 CONTINUE
      STOP
*
99999 FORMAT (1X,L5)
      END
