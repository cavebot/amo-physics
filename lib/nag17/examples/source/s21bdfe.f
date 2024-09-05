*     S21BDF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION R, RJ, X, Y, Z
      INTEGER          IFAIL, IX, IY, IZ
*     .. External Functions ..
      DOUBLE PRECISION S21BDF
      EXTERNAL         S21BDF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'S21BDF Example Program Results'
      WRITE (NOUT,*)
      WRITE (NOUT,*) '    X      Y      Z      R        S21BDF  IFAIL'
      WRITE (NOUT,*)
      DO 60 IX = 1, 3
         X = IX*0.5D0
         DO 40 IY = IX, 3
            Y = IY*0.5D0
            DO 20 IZ = IY, 3
               Z = IZ*0.5D0
               R = 2.0D0
               IFAIL = 1
*
               RJ = S21BDF(X,Y,Z,R,IFAIL)
*
               WRITE (NOUT,99999) X, Y, Z, R, RJ, IFAIL
   20       CONTINUE
   40    CONTINUE
   60 CONTINUE
      STOP
*
99999 FORMAT (1X,4F7.2,F12.4,I5)
      END
