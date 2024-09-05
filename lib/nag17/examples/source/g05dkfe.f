*     G05DKF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION X
      INTEGER          I, IFAIL
*     .. External Functions ..
      DOUBLE PRECISION G05DKF
      EXTERNAL         G05DKF
*     .. External Subroutines ..
      EXTERNAL         G05CBF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G05DKF Example Program Results'
      WRITE (NOUT,*)
      CALL G05CBF(0)
      IFAIL = 0
      DO 20 I = 1, 5
*
         X = G05DKF(2,3,IFAIL)
*
         WRITE (NOUT,99999) X
   20 CONTINUE
      STOP
*
99999 FORMAT (1X,F10.4)
      END
