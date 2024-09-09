*     D02BDF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          N, IW
      PARAMETER        (N=3,IW=14)
*     .. Local Scalars ..
      DOUBLE PRECISION PI, STIFF, TOL, X, XEND, YNORM
      INTEGER          IFAIL, IR, M
*     .. Local Arrays ..
      DOUBLE PRECISION W(N,IW), Y(N)
*     .. External Functions ..
      DOUBLE PRECISION X01AAF
      EXTERNAL         X01AAF
*     .. External Subroutines ..
      EXTERNAL         D02BDF, FCN, FCN1, OUT
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D02BDF Example Program Results'
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Simple stiff problem'
      WRITE (NOUT,*)
      M = 75
      PI = X01AAF(X)
      YNORM = 0.0D0
      TOL = 1.0D-6
      WRITE (NOUT,99999) ' Calculation with TOL =', TOL
      STIFF = 1.0D0
      IR = 0
      X = 0.0D0
      XEND = 0.3D0
      Y(1) = 1.0D0
      Y(2) = 0.0D0
      Y(3) = 0.0D0
      IFAIL = 1
*
      CALL D02BDF(X,XEND,N,Y,TOL,IR,FCN,STIFF,YNORM,W,IW,M,OUT,IFAIL)
*
      IF (IFAIL.NE.0) THEN
         WRITE (NOUT,99998) ' D02BDF fails. IFAIL =', IFAIL
      ELSE
         CALL OUT(X,Y,W,STIFF)
      END IF
*
      WRITE (NOUT,*)
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Projectile problem'
      M = 1
      TOL = 1.0D-4
      WRITE (NOUT,*)
      WRITE (NOUT,99999) ' Calculation with TOL =', TOL
      STIFF = 1.0D0
      IR = 0
      X = 0.0D0
      XEND = 8.0D0
      Y(1) = 0.0D0
      Y(2) = 0.5D0
      Y(3) = PI/5.0D0
      IFAIL = 1
*
      CALL D02BDF(X,XEND,N,Y,TOL,IR,FCN1,STIFF,YNORM,W,IW,M,OUT,IFAIL)
*
      IF (IFAIL.NE.0) WRITE (NOUT,99998) ' D02BDF fails. IFAIL =', IFAIL
      STOP
*
99999 FORMAT (1X,A,D9.1)
99998 FORMAT (1X,A,I3)
      END
*
      SUBROUTINE FCN(T,Y,F)
*     .. Parameters ..
      INTEGER        N
      PARAMETER      (N=3)
*     .. Scalar Arguments ..
      DOUBLE PRECISION T
*     .. Array Arguments ..
      DOUBLE PRECISION F(N), Y(N)
*     .. Executable Statements ..
      F(1) = -0.04D0*Y(1) + 1.0D4*Y(2)*Y(3)
      F(2) = 0.04D0*Y(1) - 1.0D4*Y(2)*Y(3) - 3.0D7*Y(2)*Y(2)
      F(3) = 3.0D7*Y(2)*Y(2)
      RETURN
      END
*
      SUBROUTINE FCN1(T,Y,F)
*     .. Parameters ..
      INTEGER         N
      PARAMETER       (N=3)
*     .. Scalar Arguments ..
      DOUBLE PRECISION T
*     .. Array Arguments ..
      DOUBLE PRECISION F(N), Y(N)
*     .. Intrinsic Functions ..
      INTRINSIC       COS, TAN
*     .. Executable Statements ..
      F(1) = TAN(Y(3))
      F(2) = -0.032D0*TAN(Y(3))/Y(2) - 0.02D0*Y(2)/COS(Y(3))
      F(3) = -0.032D0/Y(2)**2
      RETURN
      END
*
      SUBROUTINE OUT(X,Y,W,STIFF)
*     .. Parameters ..
      INTEGER        NOUT
      PARAMETER      (NOUT=6)
      INTEGER        N, IW
      PARAMETER      (N=3,IW=14)
*     .. Scalar Arguments ..
      DOUBLE PRECISION STIFF, X
*     .. Array Arguments ..
      DOUBLE PRECISION W(N,IW), Y(N)
*     .. Local Scalars ..
      INTEGER        J
*     .. Executable Statements ..
      WRITE (NOUT,*)
      WRITE (NOUT,99999) '  X and solution', X, (Y(J),J=1,N)
      WRITE (NOUT,99998) '  Current error estimates', (W(J,1),J=1,N)
      WRITE (NOUT,99998) '  Maximum error estimates', (W(J,2),J=1,N)
      WRITE (NOUT,99997) '  Number of sign changes for each estimate',
     +  (W(J,3),J=1,N)
      WRITE (NOUT,99996) '  Stiffness factor', STIFF
      RETURN
*
99999 FORMAT (1X,A,4F13.5)
99998 FORMAT (1X,A,3D12.2)
99997 FORMAT (1X,A,3F5.0)
99996 FORMAT (1X,A,F11.4)
      END
