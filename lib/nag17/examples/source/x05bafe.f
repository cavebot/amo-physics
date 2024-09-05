*     X05BAF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      DOUBLE PRECISION ONE
      PARAMETER        (ONE=1.0D0)
*     .. Local Scalars ..
      DOUBLE PRECISION CPTIME, E, S1, S2, T
      INTEGER          N
*     .. External Functions ..
      DOUBLE PRECISION X05BAF
      EXTERNAL         X05BAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'X05BAF Example Program Results'
*
      S1 = X05BAF()
*
      E = ONE
      T = ONE
      DO 20 N = 1, 10000
         T = T/N
         E = E + T
   20 CONTINUE
*
      S2 = X05BAF()
*
      CPTIME = S2 - S1
      WRITE (NOUT,99999) 'It took', CPTIME, ' seconds to compute e =', E
      STOP
*
99999 FORMAT (1X,A,1P,D10.2,A,D13.5)
      END
