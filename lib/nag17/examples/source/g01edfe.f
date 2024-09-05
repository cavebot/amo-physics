*     G01EDF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION DF1, DF2, F, PROB
      INTEGER          IFAIL
*     .. External Functions ..
      DOUBLE PRECISION G01EDF
      EXTERNAL         G01EDF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G01EDF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      WRITE (NOUT,*)
      WRITE (NOUT,*) '   F      DF1     DF2    PROB'
      WRITE (NOUT,*)
   20 READ (NIN,*,END=40) F, DF1, DF2
      IFAIL = -1
*
      PROB = G01EDF('Lower',F,DF1,DF2,IFAIL)
*
      IF (IFAIL.EQ.0) THEN
         WRITE (NOUT,99999) F, DF1, DF2, PROB
      ELSE
         WRITE (NOUT,99999) F, DF1, DF2, PROB, ' NOTE: IFAIL = ', IFAIL
      END IF
      GO TO 20
   40 STOP
*
99999 FORMAT (1X,F6.3,2F8.3,F8.4,A,I1)
      END
