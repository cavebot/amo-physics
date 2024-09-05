*     G01EAF Example Program Text
*     Mark 15 Release. NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION PROB, X
      INTEGER          I, IFAIL
      CHARACTER        TAIL
*     .. External Functions ..
      DOUBLE PRECISION G01EAF
      EXTERNAL         G01EAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G01EAF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      WRITE (NOUT,*)
      WRITE (NOUT,*) ' Tail    X      Probability '
      WRITE (NOUT,*)
      DO 20 I = 1, 4
         READ (NIN,*) TAIL, X
         IFAIL = 0
*
         PROB = G01EAF(TAIL,X,IFAIL)
*
         WRITE (NOUT,99999) TAIL, X, PROB
   20 CONTINUE
      STOP
*
99999 FORMAT (3X,A1,4X,F4.2,7X,F6.4)
      END
