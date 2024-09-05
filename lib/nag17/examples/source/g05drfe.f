*     G05DRF Example Program Text
*     Mark 15 Release. NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION A
      INTEGER          IFAIL, X
*     .. External Functions ..
      INTEGER          G05DRF
      EXTERNAL         G05DRF
*     .. External Subroutines ..
      EXTERNAL         G05CBF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G05DRF Example Program Results'
      WRITE (NOUT,*)
*     Skip heading in data file
      READ (NIN,*)
      IFAIL = 0
      CALL G05CBF(0)
   20 READ (NIN,*,END=40) A
*
      X = G05DRF(A,IFAIL)
*
      WRITE (NOUT,99999) X
      GO TO 20
   40 STOP
*
99999 FORMAT (1X,I10)
      END
