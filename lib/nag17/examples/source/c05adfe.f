*     C05ADF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION A, B, EPS, ETA, X
      INTEGER          IFAIL
*     .. External Functions ..
      DOUBLE PRECISION F
      EXTERNAL         F
*     .. External Subroutines ..
      EXTERNAL         C05ADF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'C05ADF Example Program Results'
      A = 0.0D0
      B = 1.0D0
      EPS = 1.0D-5
      ETA = 0.0D0
      IFAIL = 1
*
      CALL C05ADF(A,B,EPS,ETA,F,X,IFAIL)
*
      WRITE (NOUT,*)
      IF (IFAIL.EQ.0) THEN
         WRITE (NOUT,99999) 'Zero =', X
      ELSE
         WRITE (NOUT,99998) 'IFAIL =', IFAIL
         IF (IFAIL.EQ.2 .OR. IFAIL.EQ.3) WRITE (NOUT,99999)
     +       'Final point = ', X
      END IF
      STOP
*
99999 FORMAT (1X,A,F12.5)
99998 FORMAT (1X,A,I3)
      END
*
      DOUBLE PRECISION FUNCTION F(X)
*     .. Scalar Arguments ..
      DOUBLE PRECISION            X
*     .. Intrinsic Functions ..
      INTRINSIC                   EXP
*     .. Executable Statements ..
      F = EXP(-X) - X
      RETURN
      END
