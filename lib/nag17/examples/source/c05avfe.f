*     C05AVF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION BOUNDL, BOUNDU, FX, H, X, Y
      INTEGER          IFAIL, IND
*     .. Local Arrays ..
      DOUBLE PRECISION C(11)
*     .. External Subroutines ..
      EXTERNAL         C05AVF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'C05AVF Example Program Results'
      WRITE (NOUT,*)
      X = 3.0D0
      H = 0.1D0
      BOUNDL = 0.0D0
      BOUNDU = 4.0D0
      IFAIL = 1
      IND = 1
*
   20 CALL C05AVF(X,FX,H,BOUNDL,BOUNDU,Y,C,IND,IFAIL)
*
      IF (IND.NE.0) THEN
         FX = X*X - 3.0D0*X + 2.0D0
         GO TO 20
      ELSE
         IF (IFAIL.GT.0) THEN
            WRITE (NOUT,99997) 'Error exit,  IFAIL =', IFAIL
         ELSE
            WRITE (NOUT,*) 'Interval containing root is (Y,X) where'
            WRITE (NOUT,99999) 'Y =', Y, '   X =', X
            WRITE (NOUT,*) 'Values of f at Y and X are'
            WRITE (NOUT,99998) 'f(Y) =', C(1), '   f(X) =', FX
         END IF
      END IF
      STOP
*
99999 FORMAT (1X,A,F12.4,A,F12.4)
99998 FORMAT (1X,A,F12.2,A,F12.2)
99997 FORMAT (1X,A,I2)
      END
