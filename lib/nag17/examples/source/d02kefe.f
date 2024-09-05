*     D02KEF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          M
      PARAMETER        (M=5)
*     .. Local Scalars ..
      DOUBLE PRECISION DELAM, ELAM, TOL
      INTEGER          IFAIL, IFLAG, K, MATCH, MAXIT
*     .. Local Arrays ..
      DOUBLE PRECISION HMAX(2,M), XPOINT(M)
*     .. External Subroutines ..
      EXTERNAL         BDYVL, COEFF, D02KAY, D02KEF, REPORT
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D02KEF Example Program Results'
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'A singular problem'
      TOL = 1.0D-4
      XPOINT(1) = 0.0D0
      XPOINT(2) = 0.1D0
      XPOINT(3) = 4.0D0**(1.0D0/3.0D0)
      XPOINT(4) = 30.0D0
      XPOINT(5) = 30.0D0
      HMAX(1,1) = 0.0D0
      HMAX(1,2) = 0.0D0
      MAXIT = 0
      K = 11
      ELAM = 14.0D0
      DELAM = 1.0D0
      MATCH = 0
      IFLAG = 0
      IFAIL = 0
*     * To obtain monitoring information from the supplied
*     subroutine MONIT replace the name D02KAY by MONIT in
*     the next statement, and declare MONIT as external *
*
      CALL D02KEF(XPOINT,M,MATCH,COEFF,BDYVL,K,TOL,ELAM,DELAM,HMAX,
     +            MAXIT,IFLAG,D02KAY,REPORT,IFAIL)
*
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Final results'
      WRITE (NOUT,*)
      WRITE (NOUT,99999) 'K =', K, '  ELAM =', ELAM, '  DELAM =', DELAM
      WRITE (NOUT,99998) 'HMAX(1,M-1) =', HMAX(1,M-1),
     +  '    HMAX(1,M) =', HMAX(1,M)
      STOP
*
99999 FORMAT (1X,A,I3,A,F12.3,A,D12.2)
99998 FORMAT (1X,A,F10.3,A,F10.3)
      END
*
      SUBROUTINE COEFF(P,Q,DQDL,X,ELAM,JINT)
*     .. Scalar Arguments ..
      DOUBLE PRECISION DQDL, ELAM, P, Q, X
      INTEGER          JINT
*     .. Executable Statements ..
      P = 1.0D0
      Q = ELAM - X - 2.0D0/(X*X)
      DQDL = 1.0D0
      RETURN
      END
*
      SUBROUTINE BDYVL(XL,XR,ELAM,YL,YR)
*     .. Scalar Arguments ..
      DOUBLE PRECISION ELAM, XL, XR
*     .. Array Arguments ..
      DOUBLE PRECISION YL(3), YR(3)
*     .. Intrinsic Functions ..
      INTRINSIC        SQRT
*     .. Executable Statements ..
      YL(1) = XL
      YL(2) = 2.0D0
      YR(1) = 1.0D0
      YR(2) = -SQRT(XR-ELAM)
      RETURN
      END
*
      SUBROUTINE REPORT(X,V,JINT)
*     .. Parameters ..
      INTEGER           NOUT
      PARAMETER         (NOUT=6)
*     .. Scalar Arguments ..
      DOUBLE PRECISION  X
      INTEGER           JINT
*     .. Array Arguments ..
      DOUBLE PRECISION  V(3)
*     .. Local Scalars ..
      DOUBLE PRECISION  PYP, R, SQRTB, Y
*     .. External Functions ..
      DOUBLE PRECISION  X02AMF
      EXTERNAL          X02AMF
*     .. Intrinsic Functions ..
      INTRINSIC         COS, EXP, LOG, SIN, SQRT
*     .. Executable Statements ..
      IF (JINT.EQ.0) THEN
         WRITE (NOUT,*)
         WRITE (NOUT,*) ' Eigenfunction values'
         WRITE (NOUT,*) '       X          Y           PYP'
      END IF
      SQRTB = SQRT(V(1))
*     Avoid underflow in call of EXP
      IF (0.5D0*V(3).GE.LOG(X02AMF())) THEN
         R = EXP(0.5D0*V(3))
      ELSE
         R = 0.0D0
      END IF
      PYP = R*SQRTB*COS(0.5D0*V(2))
      Y = R/SQRTB*SIN(0.5D0*V(2))
      WRITE (NOUT,99999) X, Y, PYP
      RETURN
*
99999 FORMAT (1X,F10.3,1P,2F12.4)
      END
*
      SUBROUTINE MONIT(MAXIT,IFLAG,ELAM,FINFO)
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Scalar Arguments ..
      DOUBLE PRECISION ELAM
      INTEGER          IFLAG, MAXIT
*     .. Array Arguments ..
      DOUBLE PRECISION FINFO(15)
*     .. Local Scalars ..
      INTEGER          I
*     .. Executable Statements ..
      IF (MAXIT.EQ.-1) THEN
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Output from MONIT'
      END IF
      WRITE (NOUT,99999) MAXIT, IFLAG, ELAM, (FINFO(I),I=1,4)
      RETURN
*
99999 FORMAT (1X,2I4,F10.3,2D12.2,2F8.1)
      END
