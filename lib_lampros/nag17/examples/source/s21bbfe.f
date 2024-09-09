*     S21BBF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION RF, X, Y, Z
      INTEGER          IFAIL, IX
*     .. External Functions ..
      DOUBLE PRECISION S21BBF
      EXTERNAL         S21BBF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'S21BBF Example Program Results'
      WRITE (NOUT,*)
      WRITE (NOUT,*) '    X      Y      Z        S21BBF  IFAIL'
      WRITE (NOUT,*)
      DO 20 IX = 1, 3
         X = IX*0.5D0
         Y = (IX+1)*0.5D0
         Z = (IX+2)*0.5D0
         IFAIL = 1
*
         RF = S21BBF(X,Y,Z,IFAIL)
*
         WRITE (NOUT,99999) X, Y, Z, RF, IFAIL
   20 CONTINUE
      STOP
*
99999 FORMAT (1X,3F7.2,F12.4,I5)
      END
