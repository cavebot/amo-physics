*     D01DAF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION ABSACC, ANS, YA, YB
      INTEGER          IFAIL, NPTS
*     .. External Functions ..
      DOUBLE PRECISION FA, FB, P1, P2A, P2B
      EXTERNAL         FA, FB, P1, P2A, P2B
*     .. External Subroutines ..
      EXTERNAL         D01DAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D01DAF Example Program Results'
      YA = 0.0D0
      YB = 1.0D0
      ABSACC = 1.0D-6
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'First formulation'
      IFAIL = 1
*
      CALL D01DAF(YA,YB,P1,P2A,FA,ABSACC,ANS,NPTS,IFAIL)
*
      WRITE (NOUT,99999) 'Integral =', ANS
      WRITE (NOUT,99998) 'Number of function evaluations =', NPTS
      IF (IFAIL.GT.0) WRITE (NOUT,99997) 'IFAIL = ', IFAIL
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Second formulation'
      IFAIL = 1
*
      CALL D01DAF(YA,YB,P1,P2B,FB,ABSACC,ANS,NPTS,IFAIL)
*
      WRITE (NOUT,99999) 'Integral =', ANS
      WRITE (NOUT,99998) 'Number of function evaluations =', NPTS
      IF (IFAIL.GT.0) WRITE (NOUT,99997) 'IFAIL = ', IFAIL
      STOP
*
99999 FORMAT (1X,A,F9.4)
99998 FORMAT (1X,A,I5)
99997 FORMAT (1X,A,I2)
      END
*
      DOUBLE PRECISION FUNCTION P1(Y)
*     .. Scalar Arguments ..
      DOUBLE PRECISION             Y
*     .. Executable Statements ..
      P1 = 0.0D0
      RETURN
      END
*
      DOUBLE PRECISION FUNCTION P2A(Y)
*     .. Scalar Arguments ..
      DOUBLE PRECISION              Y
*     .. Intrinsic Functions ..
      INTRINSIC                     SQRT
*     .. Executable Statements ..
      P2A = SQRT(1.0D0-Y*Y)
      RETURN
      END
*
      DOUBLE PRECISION FUNCTION FA(X,Y)
*     .. Scalar Arguments ..
      DOUBLE PRECISION             X, Y
*     .. Executable Statements ..
      FA = X + Y
      RETURN
      END
*
      DOUBLE PRECISION FUNCTION P2B(Y)
*     .. Scalar Arguments ..
      DOUBLE PRECISION              Y
*     .. External Functions ..
      DOUBLE PRECISION              X01AAF
      EXTERNAL                      X01AAF
*     .. Executable Statements ..
      P2B = 0.5D0*X01AAF(0.0D0)
      RETURN
      END
*
      DOUBLE PRECISION FUNCTION FB(X,Y)
*     .. Scalar Arguments ..
      DOUBLE PRECISION             X, Y
*     .. Intrinsic Functions ..
      INTRINSIC                    COS, SIN
*     .. Executable Statements ..
      FB = Y*Y*(COS(X)+SIN(X))
      RETURN
      END
