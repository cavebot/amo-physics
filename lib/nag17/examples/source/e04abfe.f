*     E04ABF Example Program Text.
*     Mark 17 Revised.  NAG Copyright 1995.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION A, B, EPS, F, T, X
      INTEGER          IFAIL, MAXCAL
*     .. External Subroutines ..
      EXTERNAL         E04ABF, FUNCT
*     .. Executable Statements ..
      WRITE (NOUT,*) 'E04ABF Example Program Results'
*     EPS and T are set to zero so that E04ABF will reset them to
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
      CALL E04ABF(FUNCT,EPS,T,A,B,MAXCAL,X,F,IFAIL)
*
      WRITE (NOUT,*)
      IF (IFAIL.EQ.1) THEN
         WRITE (NOUT,*) 'Parameter outside expected range'
      ELSE
         IF (IFAIL.EQ.2) THEN
            WRITE (NOUT,*)
     +        'Results after MAXCAL function evaluations are'
            WRITE (NOUT,*)
         END IF
         WRITE (NOUT,99999) 'The minimum lies in the interval ', A,
     +     ' to ', B
         WRITE (NOUT,99999) 'Its estimated position is ', X, ','
         WRITE (NOUT,99998) 'where the function value is ', F
         WRITE (NOUT,99997) MAXCAL, 'function evaluations were required'
      END IF
      STOP
*
99999 FORMAT (1X,A,F10.8,A,F10.8)
99998 FORMAT (1X,A,F7.4)
99997 FORMAT (1X,I2,1X,A)
      END
*
      SUBROUTINE FUNCT(XC,FC)
*     Routine to evaluate F(x) at any point in (A, B)
*     .. Scalar Arguments ..
      DOUBLE PRECISION FC, XC
*     .. Intrinsic Functions ..
      INTRINSIC        SIN
*     .. Executable Statements ..
      FC = SIN(XC)/XC
      RETURN
      END
