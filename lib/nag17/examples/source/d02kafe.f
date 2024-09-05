*     D02KAF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Scalars in Common ..
      INTEGER          QQ
*     .. Local Scalars ..
      DOUBLE PRECISION DELAM, ELAM, PI, TOL, XL, XR
      INTEGER          I, IFAIL, K
*     .. Local Arrays ..
      DOUBLE PRECISION BCOND(3,2)
*     .. External Functions ..
      DOUBLE PRECISION X01AAF
      EXTERNAL         X01AAF
*     .. External Subroutines ..
      EXTERNAL         COEFFN, D02KAF, D02KAY
*     .. Common blocks ..
      COMMON           QQ
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D02KAF Example Program Results'
      PI = X01AAF(DELAM)
      XL = 0
      XR = PI
      BCOND(1,1) = 1.0D0
      BCOND(2,1) = 0.0D0
      BCOND(1,2) = 1.0D0
      BCOND(2,2) = 0.0D0
      K = 4
      QQ = 5
      DO 20 I = 5, 6
         TOL = 10.0D0**(-I)
         WRITE (NOUT,*)
         WRITE (NOUT,99999) 'Calculation with TOL =', TOL
         ELAM = 15.0D0
         DELAM = 4.0D0
         IFAIL = 1
*
*        * To obtain monitoring information from the supplied
*        subroutine MONIT replace the name D02KAY by MONIT in
*        the next statement, and declare MONIT as external *
*
         CALL D02KAF(XL,XR,COEFFN,BCOND,K,TOL,ELAM,DELAM,D02KAY,IFAIL)
*
         WRITE (NOUT,*)
         IF (IFAIL.NE.0) THEN
            WRITE (NOUT,99996) ' D02KAF fails. IFAIL =', IFAIL
         ELSE
            WRITE (NOUT,*) ' Final results'
            WRITE (NOUT,*)
            WRITE (NOUT,99998) ' K =', K, '  QQ =', QQ, '  ELAM =',
     +        ELAM, '    DELAM =', DELAM
            WRITE (NOUT,99997) ' BCOND(3,1) =', BCOND(3,1),
     +        '    BCOND(3,2) =', BCOND(3,2)
            WRITE (NOUT,*)
         END IF
   20 CONTINUE
      STOP
*
99999 FORMAT (1X,A,D16.4)
99998 FORMAT (1X,A,I3,A,I3,A,F12.3,A,D12.2)
99997 FORMAT (1X,A,D12.4,A,D12.4)
99996 FORMAT (1X,A,I3)
      END
*
      SUBROUTINE COEFFN(P,Q,DQDL,X,ELAM,JINT)
*     .. Scalar Arguments ..
      DOUBLE PRECISION  DQDL, ELAM, P, Q, X
      INTEGER           JINT
*     .. Scalars in Common ..
      INTEGER           QQ
*     .. Intrinsic Functions ..
      INTRINSIC         COS, DBLE
*     .. Common blocks ..
      COMMON            QQ
*     .. Executable Statements ..
      P = 1.0D0
      DQDL = 1.0D0
      Q = ELAM - 2.0D0*DBLE(QQ)*COS(2.0D0*X)
      RETURN
      END
*
      SUBROUTINE MONIT(NIT,IFLAG,ELAM,FINFO)
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Scalar Arguments ..
      DOUBLE PRECISION ELAM
      INTEGER          IFLAG, NIT
*     .. Array Arguments ..
      DOUBLE PRECISION FINFO(15)
*     .. Local Scalars ..
      INTEGER          I
*     .. Executable Statements ..
      IF (NIT.EQ.14) THEN
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Output from MONIT'
      END IF
      WRITE (NOUT,99999) NIT, IFLAG, ELAM, (FINFO(I),I=1,4)
      RETURN
*
99999 FORMAT (1X,2I4,F10.3,2D12.2,2F8.1)
      END
