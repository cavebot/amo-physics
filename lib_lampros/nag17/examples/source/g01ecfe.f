*     G01ECF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION DF, PROB, X
      INTEGER          IFAIL
*     .. External Functions ..
      DOUBLE PRECISION G01ECF
      EXTERNAL         G01ECF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G01ECF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      WRITE (NOUT,*)
      WRITE (NOUT,*) '   X       DF    PROB'
      WRITE (NOUT,*)
   20 READ (NIN,*,END=40) X, DF
      IFAIL = -1
*
      PROB = G01ECF('Lower',X,DF,IFAIL)
*
      IF (IFAIL.EQ.0) THEN
         WRITE (NOUT,99999) X, DF, PROB
      ELSE
         WRITE (NOUT,99999) X, DF, PROB, ' NOTE: IFAIL = ', IFAIL
      END IF
      GO TO 20
   40 STOP
*
99999 FORMAT (1X,F6.3,F8.3,F8.4,A,I1)
      END
