*     G01GBF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION DELTA, DF, PROB, T, TOL
      INTEGER          IFAIL, MAXIT
*     .. External Functions ..
      DOUBLE PRECISION G01GBF
      EXTERNAL         G01GBF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G01GBF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      WRITE (NOUT,*)
      WRITE (NOUT,*) '     T       DF    DELTA    PROB'
      WRITE (NOUT,*)
      TOL = 0.5D-5
      MAXIT = 50
   20 READ (NIN,*,END=40) T, DF, DELTA
      IFAIL = 0
*
      PROB = G01GBF(T,DF,DELTA,TOL,MAXIT,IFAIL)
*
      WRITE (NOUT,99999) T, DF, DELTA, PROB
      GO TO 20
   40 STOP
*
99999 FORMAT (1X,3F8.3,F8.4)
      END
