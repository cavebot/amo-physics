*     D01GCF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NDIM
      PARAMETER        (NDIM=4)
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION ERR, RES
      INTEGER          IFAIL, ITRANS, NPTS, NRAND
*     .. Local Arrays ..
      DOUBLE PRECISION VK(NDIM)
*     .. External Functions ..
      DOUBLE PRECISION FUNCT
      EXTERNAL         FUNCT
*     .. External Subroutines ..
      EXTERNAL         D01GCF, REGION
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D01GCF Example Program Results'
      NPTS = 2
      ITRANS = 0
      NRAND = 4
      IFAIL = 0
*
      CALL D01GCF(NDIM,FUNCT,REGION,NPTS,VK,NRAND,ITRANS,RES,ERR,IFAIL)
*
      WRITE (NOUT,*)
      WRITE (NOUT,99999) 'Result =', RES, '  Standard error =', ERR
      STOP
*
99999 FORMAT (1X,A,F13.5,A,D10.2)
      END
*
      SUBROUTINE REGION(N,X,J,A,B)
*     .. Scalar Arguments ..
      DOUBLE PRECISION  A, B
      INTEGER           J, N
*     .. Array Arguments ..
      DOUBLE PRECISION  X(N)
*     .. Executable Statements ..
      A = 0.0D0
      B = 1.0D0
      RETURN
      END
*
      DOUBLE PRECISION FUNCTION FUNCT(NDIM,X)
*     .. Scalar Arguments ..
      INTEGER                         NDIM
*     .. Array Arguments ..
      DOUBLE PRECISION                X(NDIM)
*     .. Local Scalars ..
      DOUBLE PRECISION                SUM
      INTEGER                         J
*     .. Intrinsic Functions ..
      INTRINSIC                       COS, DBLE
*     .. Executable Statements ..
      SUM = 0.0D0
      DO 20 J = 1, NDIM
         SUM = SUM + X(J)
   20 CONTINUE
      FUNCT = COS(0.5D0+2.0D0*SUM-DBLE(NDIM))
      RETURN
      END
