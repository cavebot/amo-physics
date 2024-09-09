*     G01GEF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION A, B, PROB, RLAMDA, TOL, X
      INTEGER          IFAIL, MAXIT
*     .. External Functions ..
      DOUBLE PRECISION G01GEF
      EXTERNAL         G01GEF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G01GEF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      WRITE (NOUT,*)
      WRITE (NOUT,*) '     X       A       B    RLAMDA   PROB'
      WRITE (NOUT,*)
      TOL = 0.5D-5
      MAXIT = 50
   20 READ (NIN,*,END=40) X, A, B, RLAMDA
      IFAIL = -1
*
      PROB = G01GEF(X,A,B,RLAMDA,TOL,MAXIT,IFAIL)
*
      IF (IFAIL.EQ.0) THEN
         WRITE (NOUT,99999) X, A, B, RLAMDA, PROB
      ELSE
         WRITE (NOUT,99999) X, A, B, RLAMDA, PROB, ' NOTE: IFAIL = ',
     +     IFAIL
      END IF
      GO TO 20
   40 STOP
*
99999 FORMAT (1X,4F8.3,F8.4,A,I1)
      END
