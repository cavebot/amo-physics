*     C05AJF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION EPS, ETA, X
      INTEGER          IFAIL, K, NFMAX
*     .. External Functions ..
      DOUBLE PRECISION F
      EXTERNAL         F
*     .. External Subroutines ..
      EXTERNAL         C05AJF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'C05AJF Example Program Results'
      WRITE (NOUT,*)
      DO 20 K = 3, 4
         EPS = 10.0D0**(-K)
         X = 1.0D0
         ETA = 0.0D0
         NFMAX = 200
         IFAIL = 1
*
         CALL C05AJF(X,EPS,ETA,F,NFMAX,IFAIL)
*
         IF (IFAIL.EQ.0) THEN
            WRITE (NOUT,99998) 'With EPS = ', EPS, '   root = ', X
         ELSE
            WRITE (NOUT,99999) 'IFAIL =', IFAIL
            IF (IFAIL.EQ.3 .OR. IFAIL.EQ.4) THEN
               WRITE (NOUT,99998) 'With EPS = ', EPS, ' final value = ',
     +           X
            END IF
         END IF
   20 CONTINUE
      STOP
*
99999 FORMAT (1X,A,I3)
99998 FORMAT (1X,A,D10.2,A,F14.5)
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
