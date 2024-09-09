*     S01BAF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. External Functions ..
      DOUBLE PRECISION S01BAF
      EXTERNAL         S01BAF
*     .. Local Scalars ..
      DOUBLE PRECISION X, Y
      INTEGER          IFAIL
*     .. Executable Statements ..
      WRITE (NOUT,*) 'S01BAF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      WRITE (NOUT,*)
      WRITE (NOUT,*) '       X           Y'
   20 READ (NIN,*,END=40) X
      IFAIL = 0
*
      Y = S01BAF(X,IFAIL)
*
      WRITE (NOUT,99999) X, Y
      GO TO 20
   40 STOP
*
99999 FORMAT (1X,1P,2D12.4)
      END
