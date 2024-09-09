*     G01FCF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION DF, P, X
      INTEGER          IFAIL
*     .. External Functions ..
      DOUBLE PRECISION G01FCF
      EXTERNAL         G01FCF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G01FCF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      WRITE (NOUT,*)
      WRITE (NOUT,*) '     P       DF      X'
      WRITE (NOUT,*)
   20 READ (NIN,*,END=40) P, DF
      IFAIL = -1
*
      X = G01FCF(P,DF,IFAIL)
*
      IF (IFAIL.EQ.0) THEN
         WRITE (NOUT,99999) P, DF, X
      ELSE
         WRITE (NOUT,99999) P, DF, X, ' NOTE: IFAIL = ', IFAIL
      END IF
      GO TO 20
   40 STOP
*
99999 FORMAT (1X,3F8.3,A,I1)
      END
