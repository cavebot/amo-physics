*     D01BAF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION A, ANS, B
      INTEGER          I, IFAIL
*     .. Local Arrays ..
      INTEGER          NSTOR(3)
*     .. External Functions ..
      DOUBLE PRECISION D01BAF, FUN1, FUN2, FUN3, FUN4
      EXTERNAL         D01BAF, FUN1, FUN2, FUN3, FUN4
*     .. External Subroutines ..
      EXTERNAL         D01BAW, D01BAX, D01BAY, D01BAZ
*     .. Data statements ..
      DATA             NSTOR/4, 8, 16/
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D01BAF Example Program Results'
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Gauss-Legendre example'
      DO 20 I = 1, 3
         A = 0.0D0
         B = 1.0D0
         IFAIL = 1
*
         ANS = D01BAF(D01BAZ,A,B,NSTOR(I),FUN1,IFAIL)
*
         IF (IFAIL.NE.0) THEN
            WRITE (NOUT,99998) 'IFAIL = ', IFAIL
            WRITE (NOUT,*)
         END IF
         IF (IFAIL.LE.1) WRITE (NOUT,99999) NSTOR(I),
     +       ' Points     Answer = ', ANS
   20 CONTINUE
      WRITE (NOUT,*)
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Gauss-Rational example'
      DO 40 I = 1, 3
         A = 2.0D0
         B = 0.0D0
         IFAIL = 1
*
         ANS = D01BAF(D01BAY,A,B,NSTOR(I),FUN2,IFAIL)
*
         IF (IFAIL.NE.0) THEN
            WRITE (NOUT,99998) 'IFAIL = ', IFAIL
            WRITE (NOUT,*)
         END IF
         IF (IFAIL.LE.1) WRITE (NOUT,99999) NSTOR(I),
     +       ' Points     Answer = ', ANS
   40 CONTINUE
      WRITE (NOUT,*)
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Gauss-Laguerre example'
      DO 60 I = 1, 3
         IFAIL = 1
         A = 2.0D0
         B = 1.0D0
*
         ANS = D01BAF(D01BAX,A,B,NSTOR(I),FUN3,IFAIL)
*
         IF (IFAIL.NE.0) THEN
            WRITE (NOUT,99998) 'IFAIL = ', IFAIL
            WRITE (NOUT,*)
         END IF
         IF (IFAIL.LE.1) WRITE (NOUT,99999) NSTOR(I),
     +       ' Points     Answer = ', ANS
   60 CONTINUE
      WRITE (NOUT,*)
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Gauss-Hermite  example'
      DO 80 I = 1, 3
         A = -1.0D0
         B = 3.0D0
         IFAIL = 1
*
         ANS = D01BAF(D01BAW,A,B,NSTOR(I),FUN4,IFAIL)
*
         IF (IFAIL.NE.0) THEN
            WRITE (NOUT,99998) 'IFAIL = ', IFAIL
            WRITE (NOUT,*)
         END IF
         IF (IFAIL.LE.1) WRITE (NOUT,99999) NSTOR(I),
     +       ' Points     Answer = ', ANS
   80 CONTINUE
      STOP
*
99999 FORMAT (1X,I5,A,F10.5)
99998 FORMAT (1X,A,I2)
      END
*
      DOUBLE PRECISION FUNCTION FUN1(X)
*     .. Scalar Arguments ..
      DOUBLE PRECISION               X
*     .. Executable Statements ..
      FUN1 = 4.0D0/(1.0D0+X*X)
      RETURN
      END
*
      DOUBLE PRECISION FUNCTION FUN2(X)
*     .. Scalar Arguments ..
      DOUBLE PRECISION               X
*     .. Intrinsic Functions ..
      INTRINSIC                      LOG
*     .. Executable Statements ..
      FUN2 = 1.0D0/(X*X*LOG(X))
      RETURN
      END
*
      DOUBLE PRECISION FUNCTION FUN3(X)
*     .. Scalar Arguments ..
      DOUBLE PRECISION               X
*     .. Intrinsic Functions ..
      INTRINSIC                      EXP
*     .. Executable Statements ..
      FUN3 = EXP(-X)/X
      RETURN
      END
*
      DOUBLE PRECISION FUNCTION FUN4(X)
*     .. Scalar Arguments ..
      DOUBLE PRECISION               X
*     .. Intrinsic Functions ..
      INTRINSIC                      EXP
*     .. Executable Statements ..
      FUN4 = EXP(-3.0D0*X*X-4.0D0*X-1.0D0)
      RETURN
      END
