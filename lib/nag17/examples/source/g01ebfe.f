*     G01EBF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION DF, PROB, T
      INTEGER          IFAIL
      CHARACTER        TAIL
*     .. External Functions ..
      DOUBLE PRECISION G01EBF
      EXTERNAL         G01EBF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G01EBF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      WRITE (NOUT,*)
      WRITE (NOUT,*) '   T      DF     PROB  TAIL'
      WRITE (NOUT,*)
   20 READ (NIN,*,END=40) T, DF, TAIL
      IFAIL = 0
*
      PROB = G01EBF(TAIL,T,DF,IFAIL)
*
      WRITE (NOUT,99999) T, DF, PROB, TAIL
      GO TO 20
   40 STOP
*
99999 FORMAT (1X,F6.3,F8.3,F8.4,2X,A1)
      END
