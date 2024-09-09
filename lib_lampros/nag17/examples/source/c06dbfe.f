*     C06DBF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION CALC, X
*     .. Local Arrays ..
      DOUBLE PRECISION C(4)
*     .. External Functions ..
      DOUBLE PRECISION C06DBF
      EXTERNAL         C06DBF
*     .. Data statements ..
      DATA             C/1.0D0, 1.0D0, 0.5D0, 0.25D0/
*     .. Executable Statements ..
      WRITE (NOUT,*) 'C06DBF Example Program Results'
      X = 0.5D0
      CALC = C06DBF(X,C,4,1)
      WRITE (NOUT,*)
      WRITE (NOUT,99999) 'Sum =', CALC
      STOP
*
99999 FORMAT (1X,A,F8.4)
      END
