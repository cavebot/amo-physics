*     E04LBF Example Program Text.
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          N, LH, LIW, LW
      PARAMETER        (N=4,LH=N*(N-1)/2,LIW=2,LW=7*N+N*(N-1)/2)
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION ETA, F, STEPMX, XTOL
      INTEGER          IBOUND, IFAIL, IPRINT, J, MAXCAL
*     .. Local Arrays ..
      DOUBLE PRECISION BL(N), BU(N), G(N), HESD(N), HESL(LH), W(LW),
     +                 X(N)
      INTEGER          ISTATE(N), IW(LIW)
*     .. External Subroutines ..
      EXTERNAL         E04HCF, E04HDF, E04LBF, FUNCT, HESS, MONIT
*     .. Executable Statements ..
      WRITE (NOUT,*) 'E04LBF Example Program Results'
*     Set up an arbitrary point at which to check the derivatives
      X(1) = 1.46D0
      X(2) = -0.82D0
      X(3) = 0.57D0
      X(4) = 1.21D0
*     Check the 1st derivatives
      IFAIL = 0
*
      CALL E04HCF(N,FUNCT,X,F,G,IW,LIW,W,LW,IFAIL)
*
*     Check the 2nd derivatives
      IFAIL = 0
*
      CALL E04HDF(N,FUNCT,HESS,X,G,HESL,LH,HESD,IW,LIW,W,LW,IFAIL)
*
*     Continue setting parameters for E04LBF
*     * Set IPRINT to 1 to obtain output from MONIT at each iteration *
      IPRINT = -1
      MAXCAL = 50*N
      ETA = 0.9D0
*     Set XTOL to zero so that E04LBF will use the default tolerance
      XTOL = 0.0D0
*     We estimate that the minimum will be within 4 units of the
*     starting point
      STEPMX = 4.0D0
      IBOUND = 0
      BL(1) = 1.0D0
      BU(1) = 3.0D0
      BL(2) = -2.0D0
      BU(2) = 0.0D0
*     X(3) is not bounded, so we set BL(3) to a large negative
*     number and BU(3) to a large positive number
      BL(3) = -1.0D6
      BU(3) = 1.0D6
      BL(4) = 1.0D0
      BU(4) = 3.0D0
*     Set up starting point
      X(1) = 3.0D0
      X(2) = -1.0D0
      X(3) = 0.0D0
      X(4) = 1.0D0
      IFAIL = 1
*
      CALL E04LBF(N,FUNCT,HESS,MONIT,IPRINT,MAXCAL,ETA,XTOL,STEPMX,
     +            IBOUND,BL,BU,X,HESL,LH,HESD,ISTATE,F,G,IW,LIW,W,LW,
     +            IFAIL)
*
      IF (IFAIL.NE.0) THEN
         WRITE (NOUT,*)
         WRITE (NOUT,99999) 'Error exit type', IFAIL,
     +     ' - see routine document'
      END IF
      IF (IFAIL.NE.1) THEN
         WRITE (NOUT,*)
         WRITE (NOUT,99998) 'Function value on exit is ', F
         WRITE (NOUT,99997) 'at the point', (X(J),J=1,N)
         WRITE (NOUT,*)
     +     'The corresponding (machine dependent) gradient is'
         WRITE (NOUT,99996) (G(J),J=1,N)
         WRITE (NOUT,99995) 'ISTATE contains', (ISTATE(J),J=1,N)
         WRITE (NOUT,99994) 'and HESD contains', (HESD(J),J=1,N)
      END IF
      STOP
*
99999 FORMAT (1X,A,I3,A)
99998 FORMAT (1X,A,F9.4)
99997 FORMAT (1X,A,4F9.4)
99996 FORMAT (23X,1P,4D12.3)
99995 FORMAT (1X,A,4I5)
99994 FORMAT (1X,A,4D12.4)
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
*
      SUBROUTINE MONIT(N,XC,FC,GC,ISTATE,GPJNRM,COND,POSDEF,NITER,NF,IW,
     +                 LIW,W,LW)
*     Monitoring routine
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Scalar Arguments ..
      DOUBLE PRECISION COND, FC, GPJNRM
      INTEGER          LIW, LW, N, NF, NITER
      LOGICAL          POSDEF
*     .. Array Arguments ..
      DOUBLE PRECISION GC(N), W(LW), XC(N)
      INTEGER          ISTATE(N), IW(LIW)
*     .. Local Scalars ..
      INTEGER          ISJ, J
*     .. Executable Statements ..
      WRITE (NOUT,*)
      WRITE (NOUT,*)
     +' Itn     Fn evals              Fn value            Norm of proj g
     +radient'
      WRITE (NOUT,99999) NITER, NF, FC, GPJNRM
      WRITE (NOUT,*)
      WRITE (NOUT,*)
     +  ' J           X(J)                G(J)           Status'
      DO 20 J = 1, N
         ISJ = ISTATE(J)
         IF (ISJ.GT.0) THEN
            WRITE (NOUT,99998) J, XC(J), GC(J), '    Free'
         ELSE IF (ISJ.EQ.-1) THEN
            WRITE (NOUT,99998) J, XC(J), GC(J), '    Upper Bound'
         ELSE IF (ISJ.EQ.-2) THEN
            WRITE (NOUT,99998) J, XC(J), GC(J), '    Lower Bound'
         ELSE IF (ISJ.EQ.-3) THEN
            WRITE (NOUT,99998) J, XC(J), GC(J), '    Constant'
         END IF
   20 CONTINUE
      IF (COND.NE.0.0D0) THEN
         IF (COND.GT.1.0D6) THEN
            WRITE (NOUT,*)
            WRITE (NOUT,*)
     +'Estimated condition number of projected Hessian is more than 1.0E
     ++6'
         ELSE
            WRITE (NOUT,*)
            WRITE (NOUT,99997)
     +        'Estimated condition number of projected Hessian = ', COND
         END IF
         IF ( .NOT. POSDEF) THEN
*           The following statement is included so that this MONIT
*           can also be used in conjunction with E04KDF
            WRITE (NOUT,*)
            WRITE (NOUT,*)
     +        'Projected Hessian matrix is not positive definite'
         END IF
         RETURN
      END IF
*
99999 FORMAT (1X,I3,6X,I5,2(6X,1P,D20.4))
99998 FORMAT (1X,I2,1X,1P,2D20.4,A)
99997 FORMAT (1X,A,1P,D10.2)
      END
