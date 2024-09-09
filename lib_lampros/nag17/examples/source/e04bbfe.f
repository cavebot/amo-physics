*     E04BBF Example Program Text.
*     Mark 17 Revised.  NAG Copyright 1995.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION A, B, EPS, F, G, T, X
      INTEGER          IFAIL, MAXCAL
*     .. External Subroutines ..
      EXTERNAL         E04BBF, FUNCT
*     .. Executable Statements ..
      WRITE (NOUT,*) 'E04BBF Example Program Results'
*     EPS and T are set to zero so that E04BBF will reset them to
*     their default values
      EPS = 0.0D0
      T = 0.0D0
*     The minimum is known to lie in the range (3.5, 5.0)
      A = 3.5D0
      B = 5.0D0
*     Allow 30 calls of FUNCT
      MAXCAL = 30
      IFAIL = 1
*
      CALL E04BBF(FUNCT,EPS,T,A,B,MAXCAL,X,F,G,IFAIL)
*
      WRITE (NOUT,*)
      IF (IFAIL.EQ.1) THEN
         WRITE (NOUT,*) 'Parameter outside expected range'
      ELSE
         IF (IFAIL.EQ.2) THEN
            WRITE (NOUT,*) 'Results after MAXCAL calls of FUNCT are'
            WRITE (NOUT,*)
         END IF
         WRITE (NOUT,99999) 'The minimum lies in the interval ', A,
     +     ' to ', B
         WRITE (NOUT,99999) 'Its estimated position is ', X, ','
         WRITE (NOUT,99998) 'where the function value is ', F
         WRITE (NOUT,99997) 'and the gradient is', G,
     +     ' (machine dependent)'
         WRITE (NOUT,99996) MAXCAL, ' calls of FUNCT were required'
      END IF
      STOP
*
99999 FORMAT (1X,A,F10.8,A,F10.8)
99998 FORMAT (1X,A,F7.4)
99997 FORMAT (1X,A,1P,D8.1,A)
99996 FORMAT (1X,I2,A)
      END
*
      SUBROUTINE FUNCT(XC,FC,GC)
*     Routine to evaluate F(x) and dF/dx at any point in (A, B)
*     .. Scalar Arguments ..
      DOUBLE PRECISION FC, GC, XC
*     .. Intrinsic Functions ..
      INTRINSIC        COS, SIN
*     .. Executable Statements ..
      FC = SIN(XC)/XC
      GC = (COS(XC)-FC)/XC
      RETURN
      END
