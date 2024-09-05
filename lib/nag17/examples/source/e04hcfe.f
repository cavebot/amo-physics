*     E04HCF Example Program Text.
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          N, LIW, LW
      PARAMETER        (N=4,LIW=1,LW=3*N)
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION F
      INTEGER          IFAIL, J
*     .. Local Arrays ..
      DOUBLE PRECISION G(N), W(LW), X(N)
      INTEGER          IW(LIW)
*     .. External Subroutines ..
      EXTERNAL         E04HCF, FUNCT
*     .. Executable Statements ..
      WRITE (NOUT,*) 'E04HCF Example Program Results'
*     Set up an arbitrary point at which to check the 1st derivatives
      X(1) = 1.46D0
      X(2) = -0.82D0
      X(3) = 0.57D0
      X(4) = 1.21D0
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'The test point is'
      WRITE (NOUT,99999) (X(J),J=1,N)
      IFAIL = 1
*
      CALL E04HCF(N,FUNCT,X,F,G,IW,LIW,W,LW,IFAIL)
*
      WRITE (NOUT,*)
      IF (IFAIL.LT.0) THEN
         WRITE (NOUT,99998) 'IFLAG was set to ', IFAIL, 'in FUNCT'
      ELSE IF (IFAIL.EQ.1) THEN
         WRITE (NOUT,*) 'A parameter is outside its expected range'
      ELSE
         IF (IFAIL.EQ.0) THEN
            WRITE (NOUT,*)
     +        '1st derivatives are consistent with function values'
         ELSE
            WRITE (NOUT,*)
     +        'Probable error in calculation of 1st derivatives'
         END IF
         WRITE (NOUT,*)
         WRITE (NOUT,99997)
     +     'At the test point, FUNCT gives the function value', F
         WRITE (NOUT,*) 'and the 1st derivatives'
         WRITE (NOUT,99996) (G(J),J=1,N)
      END IF
      STOP
*
99999 FORMAT (1X,4F10.4)
99998 FORMAT (1X,A,I3,A)
99997 FORMAT (1X,A,1P,D12.4)
99996 FORMAT (1X,1P,4D12.3)
      END
*
      SUBROUTINE FUNCT(IFLAG,N,XC,FC,GC,IW,LIW,W,LW)
*     Routine to evaluate objective function and its 1st derivatives.
*     .. Scalar Arguments ..
      DOUBLE PRECISION FC
      INTEGER          IFLAG, LIW, LW, N
*     .. Array Arguments ..
      DOUBLE PRECISION GC(N), W(LW), XC(N)
      INTEGER          IW(LIW)
*     .. Executable Statements ..
      IF (IFLAG.NE.1) THEN
         FC = (XC(1)+10.0D0*XC(2))**2 + 5.0D0*(XC(3)-XC(4))**2 + (XC(2)
     +        -2.0D0*XC(3))**4 + 10.0D0*(XC(1)-XC(4))**4
      END IF
      IF (IFLAG.NE.0) THEN
         GC(1) = 2.0D0*(XC(1)+10.0D0*XC(2)) + 40.0D0*(XC(1)-XC(4))**3
         GC(2) = 20.0D0*(XC(1)+10.0D0*XC(2)) + 4.0D0*(XC(2)-2.0D0*XC(3))
     +           **3
         GC(3) = 10.0D0*(XC(3)-XC(4)) - 8.0D0*(XC(2)-2.0D0*XC(3))**3
         GC(4) = 10.0D0*(XC(4)-XC(3)) - 40.0D0*(XC(1)-XC(4))**3
      END IF
      RETURN
      END
