*     D01AHF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION A, ANS, B, EPSR, RELERR
      INTEGER          IFAIL, N, NLIMIT
*     .. External Functions ..
      DOUBLE PRECISION D01AHF, FUN
      EXTERNAL         D01AHF, FUN
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D01AHF Example Program Results'
      A = 0.0D0
      B = 1.0D0
      NLIMIT = 0
      EPSR = 1.0D-5
      IFAIL = 1
      ANS = D01AHF(A,B,EPSR,N,RELERR,FUN,NLIMIT,IFAIL)
      WRITE (NOUT,*)
      IF (IFAIL.NE.0) THEN
         WRITE (NOUT,99997) 'IFAIL = ', IFAIL
         WRITE (NOUT,*)
      END IF
      IF (IFAIL.LE.2) THEN
         WRITE (NOUT,99999) 'Integral = ', ANS
         WRITE (NOUT,*)
         WRITE (NOUT,99998) 'Estimated relative error = ', RELERR
         WRITE (NOUT,*)
         WRITE (NOUT,99997) 'Number of function evaluations = ', N
      END IF
      STOP
*
99999 FORMAT (1X,A,F8.5)
99998 FORMAT (1X,A,D10.2)
99997 FORMAT (1X,A,I4)
      END
*
      DOUBLE PRECISION FUNCTION FUN(X)
*     .. Scalar Arguments ..
      DOUBLE PRECISION              X
*     .. Executable Statements ..
      FUN = 4.0D0/(1.0D0+X*X)
      RETURN
      END
