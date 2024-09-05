*     G01FBF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION DF, P, X
      INTEGER          IFAIL
      CHARACTER        TAIL
*     .. External Functions ..
      DOUBLE PRECISION G01FBF
      EXTERNAL         G01FBF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G01FBF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      WRITE (NOUT,*)
      WRITE (NOUT,*) '     P      DF    TAIL      X'
      WRITE (NOUT,*)
   20 READ (NIN,*,END=40) P, DF, TAIL
      IFAIL = -1
*
      X = G01FBF(TAIL,P,DF,IFAIL)
*
      IF (IFAIL.EQ.0) THEN
         WRITE (NOUT,99999) P, DF, TAIL, X
      ELSE
         WRITE (NOUT,99999) P, DF, TAIL, X, ' NOTE: IFAIL = ', IFAIL
      END IF
      GO TO 20
   40 STOP
*
99999 FORMAT (1X,2F8.3,3X,A1,3X,F8.3,A,I1)
      END
