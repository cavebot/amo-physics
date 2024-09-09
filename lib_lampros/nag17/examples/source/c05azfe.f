*     C05AZF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION FX, TOLX, X, Y
      INTEGER          IFAIL, IND, IR
*     .. Local Arrays ..
      DOUBLE PRECISION C(17)
*     .. External Functions ..
      DOUBLE PRECISION F
      EXTERNAL         F
*     .. External Subroutines ..
      EXTERNAL         C05AZF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'C05AZF Example Program Results'
      WRITE (NOUT,*)
      WRITE (NOUT,*) ' Iterations'
      WRITE (NOUT,*)
      TOLX = 1.0D-5
      X = 0.0D0
      Y = 1.0D0
      IR = 0
      IFAIL = 1
      IND = 1
*
   20 CALL C05AZF(X,Y,FX,TOLX,IR,C,IND,IFAIL)
*
      IF (IND.NE.0) THEN
         IF (IND.LT.2 .OR. IND.GT.4) THEN
            WRITE (NOUT,99997) 'Failure with IND=', IND, ' at X=', X
         ELSE
            FX = F(X)
            WRITE (NOUT,99999) ' X=', X, '   FX=', FX, '   IND=', IND
            GO TO 20
         END IF
      ELSE
         IF (IFAIL.EQ.0) THEN
            WRITE (NOUT,*)
            WRITE (NOUT,*) ' Solution'
            WRITE (NOUT,*)
            WRITE (NOUT,99998) ' X=', X, '   Y=', Y
         ELSE
            WRITE (NOUT,99997) 'IFAIL = ', IFAIL
            IF (IFAIL.EQ.4 .OR. IFAIL.EQ.5) WRITE (NOUT,99998) 'X =', X,
     +          '  Y =', Y
         END IF
      END IF
      STOP
*
99999 FORMAT (1X,A,F8.5,A,D12.4,A,I2)
99998 FORMAT (1X,A,F8.5,A,F8.5)
99997 FORMAT (1X,A,I2,A,F10.4)
      END
*
      DOUBLE PRECISION FUNCTION F(X)
*     .. Scalar Arguments ..
      DOUBLE PRECISION            X
*     .. Intrinsic Functions ..
      INTRINSIC                   EXP
*     .. Executable Statements ..
      F = EXP(-X) - X
      RETURN
      END
