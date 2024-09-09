*     D01JAF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION EPSA, EPSR, ESTERR, RADIUS, RELEST, RESULT
      INTEGER          ICOORD, IFAIL, ITEST, METHOD, NDIM, NEVALS
*     .. Local Arrays ..
      INTEGER          ND(2)
*     .. External Functions ..
      DOUBLE PRECISION F
      EXTERNAL         F
*     .. External Subroutines ..
      EXTERNAL         D01JAF
*     .. Data statements ..
      DATA             ND/2, 4/
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D01JAF Example Program Results'
      RADIUS = 1.0D0
      METHOD = 0
      ICOORD = 1
      EPSA = 0.0D0
      EPSR = 0.5D-4
      DO 20 ITEST = 1, 2
         NDIM = ND(ITEST)
         IFAIL = 1
*
         CALL D01JAF(F,NDIM,RADIUS,EPSA,EPSR,METHOD,ICOORD,RESULT,
     +               ESTERR,NEVALS,IFAIL)
*
         WRITE (NOUT,*)
         IF (IFAIL.NE.0) THEN
            WRITE (NOUT,99999) 'IFAIL =', IFAIL
            WRITE (NOUT,*)
         END IF
         IF (IFAIL.LE.3) THEN
            RELEST = ESTERR/RESULT
            WRITE (NOUT,99999) 'Dimension of the sphere       =', NDIM
            WRITE (NOUT,99998) 'Requested relative tolerance  =', EPSR
            WRITE (NOUT,99997) 'Approximation to the integral =', RESULT
            WRITE (NOUT,99999) 'No. of function evaluations   =', NEVALS
            WRITE (NOUT,99998) 'Estimated relative error      =', RELEST
         END IF
   20 CONTINUE
      STOP
*
99999 FORMAT (1X,A,I5)
99998 FORMAT (1X,A,D9.2)
99997 FORMAT (1X,A,F9.5)
      END
*
      DOUBLE PRECISION FUNCTION F(NDIM,X)
*     .. Scalar Arguments ..
      INTEGER                     NDIM
*     .. Array Arguments ..
      DOUBLE PRECISION            X(NDIM)
*     .. Local Scalars ..
      DOUBLE PRECISION            A, RHO
*     .. Intrinsic Functions ..
      INTRINSIC                   SQRT
*     .. Executable Statements ..
      RHO = X(1)
      F = 0.0D0
      A = (1.0D0-RHO)*(1.0D0+RHO)
      IF (A.NE.0.0D0) F = 1.0D0/SQRT(A)
      RETURN
      END
