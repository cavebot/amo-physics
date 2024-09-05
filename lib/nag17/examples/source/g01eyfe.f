*     G01EYF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION D, PROB
      INTEGER          IFAIL, N
*     .. External Functions ..
      DOUBLE PRECISION G01EYF
      EXTERNAL         G01EYF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G01EYF Example Program Results'
      WRITE (NOUT,*)
      WRITE (NOUT,*) '    D       N    One-sided probability'
      WRITE (NOUT,*)
*     Skip heading in data file
      READ (NIN,*)
   20 READ (NIN,*,END=40) N, D
      IFAIL = 0
*
      PROB = G01EYF(N,D,IFAIL)
*
      WRITE (NOUT,99999) D, N, PROB
      GO TO 20
   40 STOP
*
99999 FORMAT (1X,F7.4,2X,I4,10X,F7.4)
      END
