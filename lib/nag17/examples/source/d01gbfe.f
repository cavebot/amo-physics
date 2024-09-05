*     D01GBF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NDIM, MAXCLS, LENWRK
      PARAMETER        (NDIM=4,MAXCLS=20000,LENWRK=500)
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION ACC, EPS, FINEST
      INTEGER          IFAIL, K, MINCLS
*     .. Local Arrays ..
      DOUBLE PRECISION A(NDIM), B(NDIM), WRKSTR(LENWRK)
*     .. External Functions ..
      DOUBLE PRECISION FUNCTN
      EXTERNAL         FUNCTN
*     .. External Subroutines ..
      EXTERNAL         D01GBF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D01GBF Example Program Results'
      DO 20 K = 1, NDIM
         A(K) = 0.0D0
         B(K) = 1.0D0
   20 CONTINUE
      EPS = 0.01D0
      MINCLS = 1000
      IFAIL = 1
*
      CALL D01GBF(NDIM,A,B,MINCLS,MAXCLS,FUNCTN,EPS,ACC,LENWRK,WRKSTR,
     +            FINEST,IFAIL)
*
      WRITE (NOUT,*)
      IF (IFAIL.GT.0) THEN
         WRITE (NOUT,99999) 'D01GBF fails. IFAIL =', IFAIL
         WRITE (NOUT,*)
      END IF
      IF (IFAIL.EQ.0 .OR. IFAIL.EQ.2) THEN
         WRITE (NOUT,99998) 'Requested accuracy    = ', EPS
         WRITE (NOUT,99997) 'Estimated value       = ', FINEST
         WRITE (NOUT,99998) 'Estimated accuracy    = ', ACC
         WRITE (NOUT,99999) 'Number of evaluations = ', MINCLS
      END IF
      STOP
*
99999 FORMAT (1X,A,I5)
99998 FORMAT (1X,A,D13.2)
99997 FORMAT (1X,A,F13.5)
      END
*
      DOUBLE PRECISION FUNCTION FUNCTN(NDIM,X)
*     .. Scalar Arguments ..
      INTEGER                          NDIM
*     .. Array Arguments ..
      DOUBLE PRECISION                 X(NDIM)
*     .. Intrinsic Functions ..
      INTRINSIC                        EXP
*     .. Executable Statements ..
      FUNCTN = 4.0D0*X(1)*X(3)**2*EXP(2.0D0*X(1)*X(3))/(1.0D0+X(2)+X(4))
     +         **2
      RETURN
      END
