*     G01FEF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION A, B, P, TOL, X
      INTEGER          IFAIL
*     .. External Functions ..
      DOUBLE PRECISION G01FEF
      EXTERNAL         G01FEF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G01FEF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      WRITE (NOUT,*)
      WRITE (NOUT,*) ' Probability     A         B     Deviate'
      WRITE (NOUT,*)
   20 READ (NIN,*,END=40) P, A, B
      TOL = 0.0D0
      IFAIL = -1
*
      X = G01FEF(P,A,B,TOL,IFAIL)
*
      IF (IFAIL.EQ.0) THEN
         WRITE (NOUT,99999) P, A, B, X
      ELSE
         WRITE (NOUT,99999) P, A, B, X, ' NOTE: IFAIL = ', IFAIL
      END IF
      GO TO 20
   40 STOP
*
99999 FORMAT (1X,F9.4,2F10.3,F10.4,A,I1)
      END
