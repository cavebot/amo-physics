*     S21BCF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION RD, X, Y, Z
      INTEGER          IFAIL, IX, IY
*     .. External Functions ..
      DOUBLE PRECISION S21BCF
      EXTERNAL         S21BCF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'S21BCF Example Program Results'
      WRITE (NOUT,*)
      WRITE (NOUT,*) '    X      Y      Z        S21BCF  IFAIL'
      WRITE (NOUT,*)
      DO 40 IX = 1, 3
         X = IX*0.5D0
         DO 20 IY = IX, 3
            Y = IY*0.5D0
            Z = 1.0D0
            IFAIL = 1
*
            RD = S21BCF(X,Y,Z,IFAIL)
*
            WRITE (NOUT,99999) X, Y, Z, RD, IFAIL
   20    CONTINUE
   40 CONTINUE
      STOP
*
99999 FORMAT (1X,3F7.2,F12.4,I5)
      END
