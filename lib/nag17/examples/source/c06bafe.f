*     C06BAF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          IWORK
      PARAMETER        (IWORK=16)
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION ABSERR, ANS, ERROR, PI, R, RESULT, SEQN, SIG
      INTEGER          I, IFAIL, NCALL
*     .. Local Arrays ..
      DOUBLE PRECISION WORK(IWORK)
*     .. External Functions ..
      DOUBLE PRECISION X01AAF
      EXTERNAL         X01AAF
*     .. External Subroutines ..
      EXTERNAL         C06BAF
*     .. Intrinsic Functions ..
      INTRINSIC        DBLE
*     .. Executable Statements ..
      WRITE (NOUT,*) 'C06BAF Example Program Results'
      WRITE (NOUT,*)
      PI = X01AAF(0.0D0)
      ANS = PI**2/12.0D0
      NCALL = 0
      SIG = 1.0D0
      SEQN = 0.0D0
      WRITE (NOUT,*)
     +  '                                    Estimated       Actual'
      WRITE (NOUT,*)
     +  '   I       SEQN       RESULT        abs error        error'
      WRITE (NOUT,*)
      DO 20 I = 1, 10
         R = DBLE(I)
         SEQN = SEQN + SIG/(R**2)
         IFAIL = 1
*
         CALL C06BAF(SEQN,NCALL,RESULT,ABSERR,WORK,IWORK,IFAIL)
*
         IF (IFAIL.NE.0) THEN
            WRITE (NOUT,*)
            WRITE (NOUT,99999) 'C06BAF fails. IFAIL=', IFAIL
            STOP
         END IF
         ERROR = RESULT - ANS
         SIG = -SIG
         WRITE (NOUT,99998) I, SEQN, RESULT, ABSERR, ERROR
   20 CONTINUE
      STOP
*
99999 FORMAT (1X,A,I2)
99998 FORMAT (1X,I4,2F12.4,3X,2D14.2)
      END
