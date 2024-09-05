*     E02BCF Example Program Text.
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NC7MAX
      PARAMETER        (NC7MAX=100)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION X
      INTEGER          I, IFAIL, J, L, LEFT, M, NCAP
*     .. Local Arrays ..
      DOUBLE PRECISION C(NC7MAX), LAMDA(NC7MAX), S(4)
*     .. External Subroutines ..
      EXTERNAL         E02BCF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'E02BCF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
   20 READ (NIN,*,END=80) NCAP, M
      IF (NCAP.GT.0 .AND. NCAP+7.LE.NC7MAX) THEN
         READ (NIN,*) (LAMDA(J),J=1,NCAP+7)
         READ (NIN,*) (C(J),J=1,NCAP+3)
         WRITE (NOUT,*)
         WRITE (NOUT,*)
     +   '      X             Spline    1st deriv  2nd deriv  3rd deriv'
         DO 60 I = 1, M
            READ (NIN,*) X
            DO 40 LEFT = 1, 2
               IFAIL = 0
*
               CALL E02BCF(NCAP+7,LAMDA,C,X,LEFT,S,IFAIL)
*
               IF (LEFT.EQ.1) THEN
                  WRITE (NOUT,*)
                  WRITE (NOUT,99999) X, '  LEFT', (S(L),L=1,4)
               ELSE
                  WRITE (NOUT,99999) X, ' RIGHT', (S(L),L=1,4)
               END IF
   40       CONTINUE
   60    CONTINUE
         GO TO 20
      END IF
   80 STOP
*
99999 FORMAT (1X,D11.3,A,4D11.3)
      END
