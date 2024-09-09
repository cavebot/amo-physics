*     S14BAF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION A, P, Q, TOL, X
      INTEGER          IFAIL
*     .. External Functions ..
      DOUBLE PRECISION X02AJF
      EXTERNAL         X02AJF
*     .. External Subroutines ..
      EXTERNAL         S14BAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'S14BAF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      TOL = X02AJF()
      WRITE (NOUT,*)
      WRITE (NOUT,*) '         A           X           P           Q'
   20 READ (NIN,*,END=40) A, X
      IFAIL = 0
*
      CALL S14BAF(A,X,TOL,P,Q,IFAIL)
*
      WRITE (NOUT,99999) A, X, P, Q
      GO TO 20
   40 STOP
*
99999 FORMAT (1X,4F12.4)
      END
