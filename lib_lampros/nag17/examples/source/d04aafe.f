*     D04AAF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION HBASE, XVAL
      INTEGER          I, IFAIL, J, K, L, NDER
*     .. Local Arrays ..
      DOUBLE PRECISION DER(14), EREST(14)
*     .. External Functions ..
      DOUBLE PRECISION FUN
      EXTERNAL         FUN
*     .. External Subroutines ..
      EXTERNAL         D04AAF
*     .. Intrinsic Functions ..
      INTRINSIC        ABS
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D04AAF Example Program Results'
      WRITE (NOUT,*)
      WRITE (NOUT,*)
     +'Four separate runs to calculate the first four odd order derivati
     +ves of'
      WRITE (NOUT,*) '   FUN(X) = 0.5*exp(2.0*X-1.0) at X = 0.5.'
      WRITE (NOUT,*) 'The exact results are 1, 4, 16 and 64'
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Input parameters common to all four runs'
      WRITE (NOUT,*) '   XVAL = 0.5    NDER = -7    IFAIL = 0'
      WRITE (NOUT,*)
      HBASE = 0.5D0
      NDER = -7
      L = ABS(NDER)
      IF (NDER.GE.0) THEN
         J = 1
      ELSE
         J = 2
      END IF
      XVAL = 0.5D0
      DO 40 K = 1, 4
         IFAIL = 0
*
         CALL D04AAF(XVAL,NDER,HBASE,DER,EREST,FUN,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,99999) 'with step length', HBASE,
     +     '  the results are'
         WRITE (NOUT,*) 'Order        Derivative       Error estimate'
         DO 20 I = 1, L, J
            WRITE (NOUT,99998) I, DER(I), EREST(I)
   20    CONTINUE
         HBASE = HBASE*0.1D0
   40 CONTINUE
      STOP
*
99999 FORMAT (1X,A,F9.4,A)
99998 FORMAT (1X,I2,2D21.4)
      END
*
      DOUBLE PRECISION FUNCTION FUN(X)
*     .. Scalar Arguments ..
      DOUBLE PRECISION              X
*     .. Intrinsic Functions ..
      INTRINSIC                     EXP
*     .. Executable Statements ..
      FUN = 0.5D0*EXP(2.0D0*X-1.0D0)
      RETURN
      END
