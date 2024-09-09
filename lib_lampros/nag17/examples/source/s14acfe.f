*     S14ACF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION F, X
      INTEGER          IFAIL
*     .. External Functions ..
      DOUBLE PRECISION S14ACF
      EXTERNAL         S14ACF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'S14ACF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      WRITE (NOUT,*)
      WRITE (NOUT,*) '       X        psi(X)-log(X)'
      WRITE (NOUT,*)
   20 READ (NIN,*,END=40) X
      IFAIL = 0
*
      F = S14ACF(X,IFAIL)
*
      WRITE (NOUT,99999) X, F
      GO TO 20
   40 STOP
*
99999 FORMAT (1X,F12.4,F15.4)
      END
