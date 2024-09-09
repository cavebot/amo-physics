*     E04HDF Example Program Text.
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          N, LH, LIW, LW
      PARAMETER        (N=4,LH=N*(N-1)/2,LIW=1,LW=5*N)
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION F
      INTEGER          I, IFAIL, J, K
*     .. Local Arrays ..
      DOUBLE PRECISION G(N), HESD(N), HESL(LH), W(LW), X(N)
      INTEGER          IW(LIW)
*     .. External Subroutines ..
      EXTERNAL         E04HCF, E04HDF, FUNCT, HESS
*     .. Executable Statements ..
      WRITE (NOUT,*) 'E04HDF Example Program Results'
*     Set up an arbitrary point at which to check the derivatives
      X(1) = 1.46D0
      X(2) = -0.82D0
      X(3) = 0.57D0
      X(4) = 1.21D0
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'The test point is'
      WRITE (NOUT,99999) (X(J),J=1,N)
*     Check the 1st derivatives
      IFAIL = 0
*
      CALL E04HCF(N,FUNCT,X,F,G,IW,LIW,W,LW,IFAIL)
*
*     Check the 2nd derivatives
      IFAIL = 1
*
      CALL E04HDF(N,FUNCT,HESS,X,G,HESL,LH,HESD,IW,LIW,W,LW,IFAIL)
*
      WRITE (NOUT,*)
      IF (IFAIL.LT.0) THEN
         WRITE (NOUT,99998) 'IFLAG was set to ', IFAIL,
     +     'in FUNCT or HESS'
      ELSE IF (IFAIL.EQ.1) THEN
         WRITE (NOUT,*) 'A parameter is outside its expected range'
      ELSE
         IF (IFAIL.EQ.0) THEN
            WRITE (NOUT,*)
     +        '2nd derivatives are consistent with 1st derivatives'
         ELSE IF (IFAIL.EQ.2) THEN
            WRITE (NOUT,*)
     +        'Probable error in calculation of 2nd derivatives'
         END IF
         WRITE (NOUT,*)
         WRITE (NOUT,99997)
     +     'At the test point, FUNCT gives the function value', F
         WRITE (NOUT,*) 'and the 1st derivatives'
         WRITE (NOUT,99996) (G(J),J=1,N)
         WRITE (NOUT,*)
         WRITE (NOUT,*)
     +     'HESS gives the lower triangle of the Hessian matrix'
         WRITE (NOUT,99995) HESD(1)
         K = 1
         DO 20 I = 2, N
            WRITE (NOUT,99995) (HESL(J),J=K,K+I-2), HESD(I)
            K = K + I - 1
   20    CONTINUE
      END IF
      STOP
*
99999 FORMAT (1X,4F9.4)
99998 FORMAT (1X,A,I3,A)
99997 FORMAT (1X,A,1P,D12.4)
99996 FORMAT (1X,1P,4D12.3)
99995 FORMAT (1X,1P,4D12.3)
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
      FC = (XC(1)+10.0D0*XC(2))**2 + 5.0D0*(XC(3)-XC(4))**2 + (XC(2)
     +     -2.0D0*XC(3))**4 + 10.0D0*(XC(1)-XC(4))**4
      GC(1) = 2.0D0*(XC(1)+10.0D0*XC(2)) + 40.0D0*(XC(1)-XC(4))**3
      GC(2) = 20.0D0*(XC(1)+10.0D0*XC(2)) + 4.0D0*(XC(2)-2.0D0*XC(3))**3
      GC(3) = 10.0D0*(XC(3)-XC(4)) - 8.0D0*(XC(2)-2.0D0*XC(3))**3
      GC(4) = 10.0D0*(XC(4)-XC(3)) - 40.0D0*(XC(1)-XC(4))**3
      RETURN
      END
*
      SUBROUTINE HESS(IFLAG,N,XC,FHESL,LH,FHESD,IW,LIW,W,LW)
*     Routine to evaluate 2nd derivatives
*     .. Scalar Arguments ..
      INTEGER         IFLAG, LH, LIW, LW, N
*     .. Array Arguments ..
      DOUBLE PRECISION FHESD(N), FHESL(LH), W(LW), XC(N)
      INTEGER         IW(LIW)
*     .. Executable Statements ..
      FHESD(1) = 2.0D0 + 120.0D0*(XC(1)-XC(4))**2
      FHESD(2) = 200.0D0 + 12.0D0*(XC(2)-2.0D0*XC(3))**2
      FHESD(3) = 10.0D0 + 48.0D0*(XC(2)-2.0D0*XC(3))**2
      FHESD(4) = 10.0D0 + 120.0D0*(XC(1)-XC(4))**2
      FHESL(1) = 20.0D0
      FHESL(2) = 0.0D0
      FHESL(3) = -24.0D0*(XC(2)-2.0D0*XC(3))**2
      FHESL(4) = -120.0D0*(XC(1)-XC(4))**2
      FHESL(5) = 0.0D0
      FHESL(6) = -10.0D0
      RETURN
      END
