*     D02NCF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          NEQ, NEQMAX, NRW, NINF, ML, MU, NJCPVT, NWKJAC,
     +                 MAXORD, NY2DIM, MAXSTP, MXHNIL
      PARAMETER        (NEQ=3,NEQMAX=NEQ,NRW=50+4*NEQMAX,NINF=23,ML=1,
     +                 MU=2,NJCPVT=NEQMAX,NWKJAC=NEQMAX*(2*ML+MU+1),
     +                 MAXORD=11,NY2DIM=MAXORD+3,MAXSTP=200,MXHNIL=5)
      DOUBLE PRECISION H0, HMAX, HMIN, TCRIT
      PARAMETER        (H0=0.0D0,HMAX=10.0D0,HMIN=1.0D-10,TCRIT=0.0D0)
*     .. Local Scalars ..
      DOUBLE PRECISION H, HU, T, TCUR, TOLSF, TOUT
      INTEGER          I, IFAIL, IMXER, ITASK, ITOL, ITRACE, NITER, NJE,
     +                 NQ, NQU, NRE, NST
*     .. Local Arrays ..
      DOUBLE PRECISION ATOL(NEQMAX), CONST(6), RTOL(NEQMAX), RWORK(NRW),
     +                 WKJAC(NWKJAC), Y(NEQMAX), YDOT(NEQMAX),
     +                 YSAVE(NEQMAX,NY2DIM)
      INTEGER          INFORM(NINF), JACPVT(NJCPVT)
      LOGICAL          ALGEQU(NEQMAX)
*     .. External Subroutines ..
      EXTERNAL         D02NBY, D02NCF, D02NTF, D02NWF, D02NYF, D02NZF,
     +                 FCN, JAC, X04ABF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D02NCF Example Program Results'
      CALL X04ABF(1,NOUT)
*
*     Integrate to TOUT (ITASK=1 i.e. overshooting and internal
*     interpolation) using the blend method. Default values for the
*     array CONST are used. Employ scalar relative tolerance and vector
*     absolute tolerance. The Jacobian is evaluated by JAC.
*     MONITR subroutine replaced by NAG dummy routine D02NBY.
*
      T = 0.0D0
      TOUT = 5.0D0
      ITASK = 1
      Y(1) = 1.0D0
      Y(2) = 0.0D0
      Y(3) = 0.0D0
      ITOL = 2
      RTOL(1) = 1.0D-4
      ATOL(1) = 1.0D-7
      ATOL(2) = 1.0D-8
      ATOL(3) = 1.0D-7
      DO 20 I = 1, 6
         CONST(I) = 0.0D0
   20 CONTINUE
      IFAIL = 0
*
      CALL D02NWF(NEQMAX,NY2DIM,MAXORD,CONST,TCRIT,HMIN,HMAX,H0,MAXSTP,
     +            MXHNIL,'Average-L2',RWORK,IFAIL)
      CALL D02NTF(NEQ,NEQMAX,'Analytical',ML,MU,NWKJAC,NJCPVT,RWORK,
     +            IFAIL)
*
      WRITE (NOUT,*)
      WRITE (NOUT,*) '    X          Y(1)           Y(2)           Y(3)'
      WRITE (NOUT,99999) T, (Y(I),I=1,NEQ)
*
*     Soft fail and error messages only
      ITRACE = 0
      IFAIL = 1
*
      CALL D02NCF(NEQ,NEQMAX,T,TOUT,Y,YDOT,RWORK,RTOL,ATOL,ITOL,INFORM,
     +            FCN,YSAVE,NY2DIM,JAC,WKJAC,NWKJAC,JACPVT,NJCPVT,
     +            D02NBY,ITASK,ITRACE,IFAIL)
*
      IF (IFAIL.EQ.0) THEN
         WRITE (NOUT,99999) T, (Y(I),I=1,NEQ)
      ELSE
         WRITE (NOUT,*)
         WRITE (NOUT,99998) 'Exit D02NCF with IFAIL = ', IFAIL,
     +     '  and T = ', T
         STOP
      END IF
*     Reset TOUT and call D02NZF to override internal choice for
*     stepsize. No changes to other parameters.
      H = 0.7D0
      IFAIL = 0
*
      CALL D02NZF(NEQMAX,TCRIT,H,HMIN,HMAX,MAXSTP,MXHNIL,RWORK,IFAIL)
*
      TOUT = 10.0D0
      IFAIL = 1
*
      CALL D02NCF(NEQ,NEQMAX,T,TOUT,Y,YDOT,RWORK,RTOL,ATOL,ITOL,INFORM,
     +            FCN,YSAVE,NY2DIM,JAC,WKJAC,NWKJAC,JACPVT,NJCPVT,
     +            D02NBY,ITASK,ITRACE,IFAIL)
*
      IF (IFAIL.EQ.0) THEN
         WRITE (NOUT,99999) T, (Y(I),I=1,NEQ)
*
         CALL D02NYF(NEQ,NEQMAX,HU,H,TCUR,TOLSF,RWORK,NST,NRE,NJE,NQU,
     +               NQ,NITER,IMXER,ALGEQU,INFORM,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,99997) ' HUSED = ', HU, '  HNEXT = ', H,
     +     '  TCUR = ', TCUR
         WRITE (NOUT,99996) ' NST = ', NST, '    NRE = ', NRE,
     +     '    NJE = ', NJE
         WRITE (NOUT,99996) ' NQU = ', NQU, '    NQ  = ', NQ,
     +     '  NITER = ', NITER
         WRITE (NOUT,99995) ' Max Err Comp = ', IMXER
         WRITE (NOUT,*)
      ELSE
         WRITE (NOUT,*)
         WRITE (NOUT,99998) 'Exit D02NCF with IFAIL = ', IFAIL,
     +     '  and T = ', T
      END IF
      STOP
*
99999 FORMAT (1X,F8.3,3(F13.5,2X))
99998 FORMAT (1X,A,I2,A,D12.5)
99997 FORMAT (1X,A,D12.5,A,D12.5,A,D12.5)
99996 FORMAT (1X,A,I6,A,I6,A,I6)
99995 FORMAT (1X,A,I4)
      END
*
      SUBROUTINE FCN(NEQ,T,Y,R,IRES)
*     .. Scalar Arguments ..
      DOUBLE PRECISION T
      INTEGER        IRES, NEQ
*     .. Array Arguments ..
      DOUBLE PRECISION R(NEQ), Y(NEQ)
*     .. Executable Statements ..
      R(1) = -0.04D0*Y(1) + 1.0D4*Y(2)*Y(3)
      R(2) = 0.04D0*Y(1) - 1.0D4*Y(2)*Y(3) - 3.0D7*Y(2)*Y(2)
      R(3) = 3.0D7*Y(2)*Y(2)
      RETURN
      END
*
      SUBROUTINE JAC(NEQ,T,Y,H,D,ML,MU,P)
*     .. Scalar Arguments ..
      DOUBLE PRECISION D, H, T
      INTEGER        ML, MU, NEQ
*     .. Array Arguments ..
      DOUBLE PRECISION P(ML+MU+1,NEQ), Y(NEQ)
*     .. Local Scalars ..
      DOUBLE PRECISION HXD
*     .. Executable Statements ..
      HXD = H*D
*
      P(1,1) = 1.0D0 - HXD*(-0.04D0)
      P(2,1) = -HXD*(1.0D4*Y(3))
      P(3,1) = -HXD*(1.0D4*Y(2))
*
      P(1,2) = -HXD*(0.04D0)
      P(2,2) = 1.0D0 - HXD*(-1.0D4*Y(3)-6.0D7*Y(2))
      P(3,2) = -HXD*(-1.0D4*Y(2))
*
      P(1,3) = -HXD*(6.0D7*Y(2))
      P(2,3) = 1.0D0 - HXD*(0.0D0)
      RETURN
      END
