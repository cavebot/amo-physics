*     S21BAF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION RC, X, Y
      INTEGER          IFAIL, IX
*     .. External Functions ..
      DOUBLE PRECISION S21BAF
      EXTERNAL         S21BAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'S21BAF Example Program Results'
      WRITE (NOUT,*)
      WRITE (NOUT,*) '    X      Y        S21BAF  IFAIL'
      WRITE (NOUT,*)
      DO 20 IX = 1, 3
         X = IX*0.5D0
         Y = 1.0D0
         IFAIL = 1
*
         RC = S21BAF(X,Y,IFAIL)
*
         WRITE (NOUT,99999) X, Y, RC, IFAIL
   20 CONTINUE
      STOP
*
99999 FORMAT (1X,2F7.2,F12.4,I5)
      END
