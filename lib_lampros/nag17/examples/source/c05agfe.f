*     C05AGF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION A, B, EPS, ETA, H, X
      INTEGER          IFAIL
*     .. External Functions ..
      DOUBLE PRECISION F
      EXTERNAL         F
*     .. External Subroutines ..
      EXTERNAL         C05AGF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'C05AGF Example Program Results'
      X = 1.0D0
      H = 0.1D0
      EPS = 1.0D-5
      ETA = 0.0D0
      IFAIL = 1
*
      CALL C05AGF(X,H,EPS,ETA,F,A,B,IFAIL)
*
      WRITE (NOUT,*)
      IF (IFAIL.EQ.0) THEN
         WRITE (NOUT,99999) 'Root is ', X
         WRITE (NOUT,99998) 'Interval searched is (', A, ',', B, ')'
      ELSE
         WRITE (NOUT,99997) 'IFAIL =', IFAIL
         IF (IFAIL.EQ.3 .OR. IFAIL.EQ.4) WRITE (NOUT,99999)
     +       'Final value = ', X
      END IF
      STOP
*
99999 FORMAT (1X,A,F13.5)
99998 FORMAT (1X,A,F8.5,A,F8.5,A)
99997 FORMAT (1X,A,I3)
      END
*
      DOUBLE PRECISION FUNCTION F(X)
*     .. Scalar Arguments ..
      DOUBLE PRECISION            X
*     .. Intrinsic Functions ..
      INTRINSIC                   EXP
*     .. Executable Statements ..
      F = X - EXP(-X)
      RETURN
      END
