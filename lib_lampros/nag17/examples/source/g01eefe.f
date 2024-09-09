*     G01EEF Example Program Text
*     Mark 15 Revised.  NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION A, B, P, PDF, Q, TOL, X
      INTEGER          IFAIL
*     .. External Subroutines ..
      EXTERNAL         G01EEF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G01EEF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      WRITE (NOUT,*)
      WRITE (NOUT,*)
     +  '    X        A        B        P        Q       PDF'
      WRITE (NOUT,*)
   20 READ (NIN,*,END=40) X, A, B, TOL
      IFAIL = -1
*
      CALL G01EEF(X,A,B,TOL,P,Q,PDF,IFAIL)
*
      IF (IFAIL.EQ.0) THEN
         WRITE (NOUT,99999) X, A, B, P, Q, PDF
      ELSE
         WRITE (NOUT,99999) X, A, B, P, Q, PDF, ' NOTE: IFAIL = ',
     +     IFAIL
      END IF
      GO TO 20
   40 STOP
*
99999 FORMAT (1X,6(F7.4,2X),A,I1)
      END
