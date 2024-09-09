*     G01HAF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION PROB, RHO, X, Y
      INTEGER          IFAIL
*     .. External Functions ..
      DOUBLE PRECISION G01HAF
      EXTERNAL         G01HAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G01HAF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      WRITE (NOUT,*)
      WRITE (NOUT,*) '     X       Y      RHO    PROB'
      WRITE (NOUT,*)
   20 READ (NIN,*,END=40) X, Y, RHO
      IFAIL = 0
*
      PROB = G01HAF(X,Y,RHO,IFAIL)
*
      WRITE (NOUT,99999) X, Y, RHO, PROB
      GO TO 20
   40 STOP
*
99999 FORMAT (1X,3F8.3,F8.4)
      END
