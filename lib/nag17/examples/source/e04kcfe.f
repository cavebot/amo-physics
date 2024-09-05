*     E04KCF Example Program Text.
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          N, LIW, LW
      PARAMETER        (N=4,LIW=N+2,LW=N*(N+7))
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION F
      INTEGER          IBOUND, IFAIL, J
*     .. Local Arrays ..
      DOUBLE PRECISION BL(N), BU(N), G(N), W(LW), X(N)
      INTEGER          IW(LIW)
*     .. External Subroutines ..
      EXTERNAL         E04KCF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'E04KCF Example Program Results'
      X(1) = 3.0D0
      X(2) = -1.0D0
      X(3) = 0.0D0
      X(4) = 1.0D0
      IBOUND = 0
      BL(1) = 1.0D0
      BU(1) = 3.0D0
      BL(2) = -2.0D0
      BU(2) = 0.0D0
*     X(3) is unconstrained, so we set BL(3) to a large negative
*     number and BU(3) to a large positive number.
      BL(3) = -1.0D6
      BU(3) = 1.0D6
      BL(4) = 1.0D0
      BU(4) = 3.0D0
      IFAIL = 1
*
      CALL E04KCF(N,IBOUND,BL,BU,X,F,G,IW,LIW,W,LW,IFAIL)
*
      IF (IFAIL.NE.0) THEN
         WRITE (NOUT,*)
         WRITE (NOUT,99999) 'Error exit type', IFAIL,
     +     ' - see routine document'
      END IF
      IF (IFAIL.NE.1) THEN
         WRITE (NOUT,*)
         WRITE (NOUT,99998) 'Function value on exit is ', F
         WRITE (NOUT,99998) 'at the point', (X(J),J=1,N)
         WRITE (NOUT,*)
     +     'the corresponding (machine dependent) gradient is'
         WRITE (NOUT,99997) (G(J),J=1,N)
      END IF
      STOP
*
99999 FORMAT (1X,A,I3,A)
99998 FORMAT (1X,A,4F12.4)
99997 FORMAT (13X,4D12.4)
      END
*
      SUBROUTINE FUNCT2(N,XC,FC,GC)
*     Routine to evaluate objective function and its 1st derivatives.
*     This routine must be called FUNCT2.
*     .. Scalar Arguments ..
      DOUBLE PRECISION  FC
      INTEGER           N
*     .. Array Arguments ..
      DOUBLE PRECISION  GC(N), XC(N)
*     .. Local Scalars ..
      DOUBLE PRECISION  X1, X2, X3, X4
*     .. Executable Statements ..
      X1 = XC(1)
      X2 = XC(2)
      X3 = XC(3)
      X4 = XC(4)
      FC = (X1+10.0D0*X2)**2 + 5.0D0*(X3-X4)**2 + (X2-2.0D0*X3)**4 +
     +     10.0D0*(X1-X4)**4
      GC(1) = 2.0D0*(X1+10.0D0*X2) + 40.0D0*(X1-X4)**3
      GC(2) = 20.0D0*(X1+10.0D0*X2) + 4.0D0*(X2-2.0D0*X3)**3
      GC(3) = 10.0D0*(X3-X4) - 8.0D0*(X2-2.0D0*X3)**3
      GC(4) = -10.0D0*(X3-X4) - 40.0D0*(X1-X4)**3
      RETURN
      END
