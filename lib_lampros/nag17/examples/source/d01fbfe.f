*     D01FBF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NDIM, LWAMAX
      PARAMETER        (NDIM=4,LWAMAX=16)
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION A, ANS, B
      INTEGER          I, IFAIL, ITYPE, IW, LWA
*     .. Local Arrays ..
      DOUBLE PRECISION ABSCIS(LWAMAX), WEIGHT(LWAMAX)
      INTEGER          NPTVEC(NDIM)
*     .. External Functions ..
      DOUBLE PRECISION D01FBF, FUN
      EXTERNAL         D01FBF, FUN
*     .. External Subroutines ..
      EXTERNAL         D01BAW, D01BAX, D01BAY, D01BAZ, D01BBF
*     .. Data statements ..
      DATA             NPTVEC/4, 4, 4, 4/
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D01FBF Example Program Results'
      LWA = 0
      DO 20 I = 1, NDIM
         LWA = LWA + NPTVEC(I)
   20 CONTINUE
      IF (LWAMAX.GE.LWA) THEN
         ITYPE = 1
         IW = 1
         A = 1.0D0
         B = 2.0D0
         IFAIL = 0
*
         CALL D01BBF(D01BAZ,A,B,ITYPE,NPTVEC(1),WEIGHT(IW),ABSCIS(IW),
     +               IFAIL)
*
         IW = IW + NPTVEC(1)
         A = 0.0D0
         B = 2.0D0
*
         CALL D01BBF(D01BAX,A,B,ITYPE,NPTVEC(2),WEIGHT(IW),ABSCIS(IW),
     +               IFAIL)
*
         IW = IW + NPTVEC(2)
         A = 0.0D0
         B = 0.5D0
*
         CALL D01BBF(D01BAW,A,B,ITYPE,NPTVEC(3),WEIGHT(IW),ABSCIS(IW),
     +               IFAIL)
*
         IW = IW + NPTVEC(3)
         A = 1.0D0
         B = 2.0D0
*
         CALL D01BBF(D01BAY,A,B,ITYPE,NPTVEC(4),WEIGHT(IW),ABSCIS(IW),
     +               IFAIL)
*
         IFAIL = 0
*
         ANS = D01FBF(NDIM,NPTVEC,LWA,WEIGHT,ABSCIS,FUN,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,99999) 'Answer = ', ANS
      END IF
      STOP
*
99999 FORMAT (1X,A,F10.5)
      END
*
      DOUBLE PRECISION FUNCTION FUN(NDIM,X)
*     .. Scalar Arguments ..
      INTEGER                       NDIM
*     .. Array Arguments ..
      DOUBLE PRECISION              X(NDIM)
*     .. Intrinsic Functions ..
      INTRINSIC                     EXP
*     .. Executable Statements ..
      FUN = (X(1)*X(2)*X(3))**6/(X(4)+2.0D0)**8*EXP(-2.0D0*X(2)
     +      -0.5D0*X(3)*X(3))
      RETURN
      END
