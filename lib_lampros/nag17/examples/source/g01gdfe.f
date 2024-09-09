*     G01GDF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION DF1, DF2, F, PROB, RLAMDA, TOL
      INTEGER          IFAIL, MAXIT
*     .. External Functions ..
      DOUBLE PRECISION G01GDF
      EXTERNAL         G01GDF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G01GDF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      WRITE (NOUT,*)
      WRITE (NOUT,*) '     F      DF1     DF2   RLAMDA   PROB'
      WRITE (NOUT,*)
      TOL = 0.5D-5
      MAXIT = 50
   20 READ (NIN,*,END=40) F, DF1, DF2, RLAMDA
      IFAIL = -1
*
      PROB = G01GDF(F,DF1,DF2,RLAMDA,TOL,MAXIT,IFAIL)
*
      IF (IFAIL.EQ.0) THEN
         WRITE (NOUT,99999) F, DF1, DF2, RLAMDA, PROB
      ELSE
         WRITE (NOUT,99999) F, DF1, DF2, RLAMDA, PROB,
     +     ' NOTE: IFAIL = ', IFAIL
      END IF
      GO TO 20
   40 STOP
*
99999 FORMAT (1X,4F8.3,F8.4,A,I1)
      END
