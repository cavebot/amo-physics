*     G01FDF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION DF1, DF2, F, P
      INTEGER          IFAIL
*     .. External Functions ..
      DOUBLE PRECISION G01FDF
      EXTERNAL         G01FDF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G01FDF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      WRITE (NOUT,*)
      WRITE (NOUT,*) '     P      DF1     DF2      F'
      WRITE (NOUT,*)
   20 READ (NIN,*,END=40) P, DF1, DF2
      IFAIL = -1
*
      F = G01FDF(P,DF1,DF2,IFAIL)
*
      IF (IFAIL.EQ.0) THEN
         WRITE (NOUT,99999) P, DF1, DF2, F
      ELSE
         WRITE (NOUT,99999) P, DF1, DF2, F, ' NOTE: IFAIL = ', IFAIL
      END IF
      GO TO 20
   40 STOP
*
99999 FORMAT (1X,4F8.3,A,I1)
      END
