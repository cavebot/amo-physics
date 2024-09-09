*     D02NHF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          NEQ, NEQMAX, NRW, NINF, ML, MU, NJCPVT, NWKJAC,
     +                 MAXORD, NY2DIM, MAXSTP, MXHNIL
      PARAMETER        (NEQ=3,NEQMAX=NEQ,NRW=50+4*NEQMAX,NINF=23,ML=1,
     +                 MU=2,NJCPVT=NEQMAX,NWKJAC=NEQMAX*(2*ML+MU+1),
     +                 MAXORD=5,NY2DIM=MAXORD+1,MAXSTP=200,MXHNIL=5)
      DOUBLE PRECISION H0, HMAX, HMIN, TCRIT
      PARAMETER        (H0=1.0D-4,HMAX=10.0D0,HMIN=1.0D-10,TCRIT=0.0D0)
      LOGICAL          PETZLD
      PARAMETER        (PETZLD=.FALSE.)
*     .. Local Scalars ..
      DOUBLE PRECISION H, HU, T, TCUR, TOLSF, TOUT
      INTEGER          I, IFAIL, IMXER, ITASK, ITOL, ITRACE, NITER, NJE,
     +                 NQ, NQU, NRE, NST
*     .. Local Arrays ..
      DOUBLE PRECISION ATOL(NEQMAX), CONST(6), RTOL(NEQMAX), RWORK(NRW),
     +                 WKJAC(NWKJAC), Y(NEQMAX), YDOT(NEQMAX),
     +                 YSAVE(NEQMAX,NY2DIM)
      INTEGER          INFORM(NINF), JACPVT(NJCPVT)
      LOGICAL          ALGEQU(NEQMAX), LDERIV(2)
*     .. External Subroutines ..
      EXTERNAL         D02NBY, D02NHF, D02NHZ, D02NTF, D02NVF, D02NYF,
     +                 RESID, X04ABF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D02NHF Example Program Results'
      CALL X04ABF(1,NOUT)
*
*     Set ITASK=6 to provide initial estimates of solution and its
*     time derivative. Default values for the array CONST are used.
*     Use the B.D.F. formulae with a Newton method.
*     Employ scalar relative tolerance and vector absolute tolerance.
*     The Jacobian is evaluated internally.
*     MONITR subroutine replaced by NAG dummy routine D02NBY.
*
      T = 0.0D0
      TOUT = 10.0D0
      ITASK = 6
      Y(1) = 1.0D0
      Y(2) = 0.0D0
      Y(3) = 0.0D0
      LDERIV(1) = .FALSE.
      LDERIV(2) = .FALSE.
      ITOL = 2
      RTOL(1) = 1.0D-4
      ATOL(1) = 1.0D-6
      ATOL(2) = 1.0D-7
      ATOL(3) = 1.0D-6
      DO 20 I = 1, 6
         CONST(I) = 0.0D0
   20 CONTINUE
      IFAIL = 0
*
      CALL D02NVF(NEQMAX,NY2DIM,MAXORD,'Newton',PETZLD,CONST,TCRIT,HMIN,
     +            HMAX,H0,MAXSTP,MXHNIL,'Average-L2',RWORK,IFAIL)
*
      CALL D02NTF(NEQ,NEQMAX,'Numerical',ML,MU,NWKJAC,NJCPVT,RWORK,
     +            IFAIL)
*
*     Soft fail and error messages only
      ITRACE = 0
      IFAIL = 1
*
      CALL D02NHF(NEQ,NEQMAX,T,TOUT,Y,YDOT,RWORK,RTOL,ATOL,ITOL,INFORM,
     +            RESID,YSAVE,NY2DIM,D02NHZ,WKJAC,NWKJAC,JACPVT,NJCPVT,
     +            D02NBY,LDERIV,ITASK,ITRACE,IFAIL)
*
      WRITE (NOUT,*)
      IF (IFAIL.EQ.0) THEN
         WRITE (NOUT,99999) ' Initial    Y : ', (Y(I),I=1,NEQ)
         WRITE (NOUT,99999) ' Initial YDOT : ', (YDOT(I),I=1,NEQ)
      ELSE
         WRITE (NOUT,99997) 'Exit D02NHF with IFAIL = ', IFAIL,
     +     '  and T = ', T
         STOP
      END IF
*     Use these initial estimates and integrate to TOUT (overshoot and
*     interpolate)
      ITASK = 1
      IFAIL = 1
      TOUT = 10.0D0
*
      CALL D02NHF(NEQ,NEQMAX,T,TOUT,Y,YDOT,RWORK,RTOL,ATOL,ITOL,INFORM,
     +            RESID,YSAVE,NY2DIM,D02NHZ,WKJAC,NWKJAC,JACPVT,NJCPVT,
     +            D02NBY,LDERIV,ITASK,ITRACE,IFAIL)
*
      IF (IFAIL.EQ.0) THEN
         WRITE (NOUT,*)
         WRITE (NOUT,*)
     +     '    X          Y(1)           Y(2)           Y(3)'
         WRITE (NOUT,99998) TOUT, (Y(I),I=1,NEQ)
*
         CALL D02NYF(NEQ,NEQMAX,HU,H,TCUR,TOLSF,RWORK,NST,NRE,NJE,NQU,
     +               NQ,NITER,IMXER,ALGEQU,INFORM,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,99996) ' HUSED = ', HU, '  HNEXT = ', H,
     +     '  TCUR = ', TCUR
         WRITE (NOUT,99995) ' NST = ', NST, '    NRE = ', NRE,
     +     '    NJE = ', NJE
         WRITE (NOUT,99995) ' NQU = ', NQU, '    NQ  = ', NQ,
     +     '  NITER = ', NITER
         WRITE (NOUT,99994) ' Max err comp = ', IMXER
      ELSE
         WRITE (NOUT,*)
         WRITE (NOUT,99997) 'Exit D02NHF with IFAIL = ', IFAIL,
     +     '  and T = ', T
      END IF
      STOP
*
99999 FORMAT (1X,A,3(F11.4,2X))
99998 FORMAT (1X,F8.3,3(F13.5,2X))
99997 FORMAT (1X,A,I2,A,D12.5)
99996 FORMAT (1X,A,D12.5,A,D12.5,A,D12.5)
99995 FORMAT (1X,A,I6,A,I6,A,I6)
99994 FORMAT (1X,A,I4)
      END
*
      SUBROUTINE RESID(NEQ,T,Y,YDOT,R,IRES)
*     .. Scalar Arguments ..
      DOUBLE PRECISION T
      INTEGER          IRES, NEQ
*     .. Array Arguments ..
      DOUBLE PRECISION R(NEQ), Y(NEQ), YDOT(NEQ)
*     .. Executable Statements ..
      R(1) = -YDOT(1) - YDOT(2) - YDOT(3)
      R(2) = -YDOT(2)
      R(3) = -YDOT(3)
      IF (IRES.EQ.1) THEN
         R(1) = 0.0D0 + R(1)
         R(2) = 0.04D0*Y(1) - 1.0D4*Y(2)*Y(3) - 3.0D7*Y(2)*Y(2) + R(2)
         R(3) = 3.0D7*Y(2)*Y(2) + R(3)
      END IF
      RETURN
      END
