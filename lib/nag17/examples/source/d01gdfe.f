*     D01GDF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          NDIM
      PARAMETER        (NDIM=4)
*     .. Local Scalars ..
      DOUBLE PRECISION ERR, RES
      INTEGER          IFAIL, ITRANS, NPTS, NRAND
*     .. Local Arrays ..
      DOUBLE PRECISION VK(NDIM)
*     .. External Subroutines ..
      EXTERNAL         D01GDF, VECFUN, VECREG
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D01GDF Example Program Results'
      WRITE (NOUT,*)
      NPTS = 2
      ITRANS = 0
      NRAND = 4
      IFAIL = 0
*
      CALL D01GDF(NDIM,VECFUN,VECREG,NPTS,VK,NRAND,ITRANS,RES,ERR,IFAIL)
*
      WRITE (NOUT,99999) 'Result = ', RES, ', standard error = ', ERR
      STOP
*
99999 FORMAT (1X,A,F13.5,A,D10.2)
      END
*
      SUBROUTINE VECFUN(NDIM,X,FV,M)
*     .. Scalar Arguments ..
      INTEGER           M, NDIM
*     .. Array Arguments ..
      DOUBLE PRECISION  FV(M), X(M,NDIM)
*     .. Local Scalars ..
      INTEGER           I, J
*     .. Intrinsic Functions ..
      INTRINSIC         COS, DBLE
*     .. Executable Statements ..
      DO 20 I = 1, M
         FV(I) = 0.0D0
   20 CONTINUE
      DO 60 J = 1, NDIM
         DO 40 I = 1, M
            FV(I) = FV(I) + X(I,J)
   40    CONTINUE
   60 CONTINUE
      DO 80 I = 1, M
         FV(I) = COS(0.5D0+2.0D0*FV(I)-DBLE(NDIM))
   80 CONTINUE
      RETURN
      END
*
      SUBROUTINE VECREG(NDIM,X,J,C,D,M)
*     .. Scalar Arguments ..
      INTEGER           J, M, NDIM
*     .. Array Arguments ..
      DOUBLE PRECISION  C(M), D(M), X(M,NDIM)
*     .. Local Scalars ..
      INTEGER           I
*     .. Executable Statements ..
      DO 20 I = 1, M
         C(I) = 0.0D0
         D(I) = 1.0D0
   20 CONTINUE
      RETURN
      END
