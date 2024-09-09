*     D01FDF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION R0, RESULT, SIGMA, U
      INTEGER          IFAIL, LIMIT, NCALLS, NDIM
*     .. External Functions ..
      DOUBLE PRECISION FUNCTN
      EXTERNAL         FUNCTN
*     .. External Subroutines ..
      EXTERNAL         D01FDF, D01FDV, REGION
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D01FDF Example Program Results'
      NDIM = 3
      LIMIT = 8000
      U = 1.5D0
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Sphere-to-sphere transformation'
      SIGMA = 1.5D0
      R0 = 0.9D0
      IFAIL = 0
*
      CALL D01FDF(NDIM,FUNCTN,SIGMA,D01FDV,LIMIT,R0,U,RESULT,NCALLS,
     +            IFAIL)
*
      WRITE (NOUT,*)
      WRITE (NOUT,99999) 'Estimated value of the integral =', RESULT
      WRITE (NOUT,99998) 'Number of integrand evaluations =', NCALLS
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Product region transformation'
      SIGMA = -1.0D0
      R0 = 0.8D0
      IFAIL = 0
*
      CALL D01FDF(NDIM,FUNCTN,SIGMA,REGION,LIMIT,R0,U,RESULT,NCALLS,
     +            IFAIL)
*
      WRITE (NOUT,*)
      WRITE (NOUT,99999) 'Estimated value of the integral =', RESULT
      WRITE (NOUT,99998) 'Number of integrand evaluations =', NCALLS
      STOP
*
99999 FORMAT (1X,A,F9.3)
99998 FORMAT (1X,A,I4)
      END
*
      DOUBLE PRECISION FUNCTION FUNCTN(NDIM,X)
*     .. Scalar Arguments ..
      INTEGER                          NDIM
*     .. Array Arguments ..
      DOUBLE PRECISION                 X(NDIM)
*     .. Local Scalars ..
      INTEGER                          I
*     .. Intrinsic Functions ..
      INTRINSIC                        ABS, SQRT
*     .. Executable Statements ..
      FUNCTN = 2.25D0
      DO 20 I = 1, NDIM
         FUNCTN = FUNCTN - X(I)*X(I)
   20 CONTINUE
      FUNCTN = 1.0D0/SQRT(ABS(FUNCTN))
      RETURN
      END
*
      SUBROUTINE REGION(NDIM,X,J,C,D)
*     .. Scalar Arguments ..
      DOUBLE PRECISION  C, D
      INTEGER           J, NDIM
*     .. Array Arguments ..
      DOUBLE PRECISION  X(NDIM)
*     .. Local Scalars ..
      DOUBLE PRECISION  SUM
      INTEGER           I, J1
*     .. Intrinsic Functions ..
      INTRINSIC         ABS, SQRT
*     .. Executable Statements ..
      C = -1.5D0
      D = 1.5D0
      IF (J.GT.1) THEN
         SUM = 2.25D0
         J1 = J - 1
         DO 20 I = 1, J1
            SUM = SUM - X(I)*X(I)
   20    CONTINUE
         D = SQRT(ABS(SUM))
         C = -D
      END IF
      RETURN
      END
