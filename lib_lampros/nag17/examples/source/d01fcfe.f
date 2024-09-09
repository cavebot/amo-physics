*     D01FCF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NDIM, MAXPTS, LENWRK
      PARAMETER        (NDIM=4,MAXPTS=1000*NDIM,LENWRK=(NDIM+2)
     +                 *(1+MAXPTS/(2**NDIM+2*NDIM*NDIM+2*NDIM+1)))
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION ACC, EPS, FINVAL
      INTEGER          IFAIL, K, MINPTS
*     .. Local Arrays ..
      DOUBLE PRECISION A(NDIM), B(NDIM), WRKSTR(LENWRK)
*     .. External Functions ..
      DOUBLE PRECISION FUNCTN
      EXTERNAL         FUNCTN
*     .. External Subroutines ..
      EXTERNAL         D01FCF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D01FCF Example Program Results'
      DO 20 K = 1, NDIM
         A(K) = 0.0D0
         B(K) = 1.0D0
   20 CONTINUE
      EPS = 0.0001D0
      MINPTS = 0
      IFAIL = 1
*
      CALL D01FCF(NDIM,A,B,MINPTS,MAXPTS,FUNCTN,EPS,ACC,LENWRK,WRKSTR,
     +            FINVAL,IFAIL)
*
      WRITE (NOUT,*)
      IF (IFAIL.NE.0) THEN
         WRITE (NOUT,99999) 'IFAIL =', IFAIL
         WRITE (NOUT,*)
      END IF
      IF (IFAIL.EQ.0 .OR. IFAIL.GE.2) THEN
         WRITE (NOUT,99998) 'Requested accuracy = ', EPS
         WRITE (NOUT,99997) 'Estimated value    = ', FINVAL
         WRITE (NOUT,99998) 'Estimated accuracy = ', ACC
      END IF
      STOP
*
99999 FORMAT (1X,A,I5)
99998 FORMAT (1X,A,D12.2)
99997 FORMAT (1X,A,F12.4)
      END
*
      DOUBLE PRECISION FUNCTION FUNCTN(NDIM,Z)
*     .. Scalar Arguments ..
      INTEGER                          NDIM
*     .. Array Arguments ..
      DOUBLE PRECISION                 Z(NDIM)
*     .. Intrinsic Functions ..
      INTRINSIC                        EXP
*     .. Executable Statements ..
      FUNCTN = 4.0D0*Z(1)*Z(3)*Z(3)*EXP(2.0D0*Z(1)*Z(3))/(1.0D0+Z(2)
     +         +Z(4))**2
      RETURN
      END
