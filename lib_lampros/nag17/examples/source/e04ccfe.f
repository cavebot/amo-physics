*     E04CCF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          N, IW
      PARAMETER        (N=2,IW=N+1)
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Scalars in Common ..
      INTEGER          IMONIT
*     .. Local Scalars ..
      DOUBLE PRECISION F, TOL
      INTEGER          I, IFAIL, MAXCAL
*     .. Local Arrays ..
      DOUBLE PRECISION SIM(IW,N), W1(N), W2(N), W3(N), W4(N), W5(IW),
     +                 X(N)
*     .. External Functions ..
      DOUBLE PRECISION X02AJF
      EXTERNAL         X02AJF
*     .. External Subroutines ..
      EXTERNAL         E04CCF, FUNCT, MONIT
*     .. Intrinsic Functions ..
      INTRINSIC        SQRT
*     .. Common blocks ..
      COMMON           /OUTP/IMONIT
*     .. Executable Statements ..
      WRITE (NOUT,*) 'E04CCF Example Program Results'
*     ** Set IMONIT to 1 to obtain monitoring information **
      IMONIT = 0
      X(1) = -1.0D0
      X(2) = 1.0D0
      TOL = SQRT(X02AJF())
      MAXCAL = 100
      IFAIL = 0
*
      CALL E04CCF(N,X,F,TOL,IW,W1,W2,W3,W4,W5,SIM,FUNCT,MONIT,MAXCAL,
     +            IFAIL)
*
      WRITE (NOUT,*)
      WRITE (NOUT,99999) 'Final function value is ', F
      WRITE (NOUT,99999) 'at the point', (X(I),I=1,N)
      WRITE (NOUT,99998) 'This has error number', IFAIL
      STOP
*
99999 FORMAT (1X,A,2F12.4)
99998 FORMAT (1X,A,I3)
      END
*
      SUBROUTINE FUNCT(N,XC,FC)
*     .. Scalar Arguments ..
      DOUBLE PRECISION FC
      INTEGER          N
*     .. Array Arguments ..
      DOUBLE PRECISION XC(N)
*     .. Intrinsic Functions ..
      INTRINSIC        EXP
*     .. Executable Statements ..
      FC = EXP(XC(1))*(4.0D0*XC(1)*(XC(1)+XC(2))+2.0D0*XC(2)*(XC(2)
     +     +1.0D0)+1.0D0)
      RETURN
      END
*
      SUBROUTINE MONIT(FMIN,FMAX,SIM,N,N1,NCALL)
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Scalar Arguments ..
      DOUBLE PRECISION FMAX, FMIN
      INTEGER          N, N1, NCALL
*     .. Array Arguments ..
      DOUBLE PRECISION SIM(N1,N)
*     .. Scalars in Common ..
      INTEGER          IMONIT
*     .. Local Scalars ..
      INTEGER          I, J
*     .. Common blocks ..
      COMMON           /OUTP/IMONIT
*     .. Executable Statements ..
      IF (IMONIT.NE.0) THEN
         WRITE (NOUT,99999) 'After', NCALL,
     +     '  function calls, the value is', FMIN, ' with simplex'
         WRITE (NOUT,99998) ((SIM(I,J),J=1,N),I=1,N1)
      END IF
      RETURN
*
99999 FORMAT (1X,A,I5,A,F10.4,A)
99998 FORMAT (1X,2F12.4)
      END
