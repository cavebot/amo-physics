*     D02NJF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          NEQ, NEQMAX, NRW, NINF, NELTS, NJCPVT, NWKJAC,
     +                 NIA, NJA, MAXORD, NY2DIM, MAXSTP, MXHNIL
      PARAMETER        (NEQ=3,NEQMAX=NEQ,NRW=50+4*NEQMAX,NINF=23,
     +                 NELTS=8,NJCPVT=150,NWKJAC=100,NIA=NEQMAX+1,
     +                 NJA=NELTS,MAXORD=5,NY2DIM=MAXORD+1,MAXSTP=200,
     +                 MXHNIL=5)
      DOUBLE PRECISION H0, HMAX, HMIN, TCRIT
      PARAMETER        (H0=0.0D0,HMAX=10.0D0,HMIN=1.0D-10,TCRIT=0.0D0)
      LOGICAL          PETZLD
      PARAMETER        (PETZLD=.TRUE.)
      DOUBLE PRECISION ETA, U, SENS
      PARAMETER        (ETA=1.0D-4,U=0.1D0,SENS=1.0D-6)
      LOGICAL          LBLOCK
      PARAMETER        (LBLOCK=.TRUE.)
*     .. Local Scalars ..
      DOUBLE PRECISION H, HU, T, TCUR, TOLSF, TOUT
      INTEGER          I, ICALL, IFAIL, IGROW, IMXER, ISPLIT, ITASK,
     +                 ITOL, ITRACE, LIWREQ, LIWUSD, LRWREQ, LRWUSD,
     +                 NBLOCK, NGP, NITER, NJE, NLU, NNZ, NQ, NQU, NRE,
     +                 NST
*     .. Local Arrays ..
      DOUBLE PRECISION ATOL(NEQMAX), CONST(6), RTOL(NEQMAX), RWORK(NRW),
     +                 WKJAC(NWKJAC), Y(NEQMAX), YDOT(NEQMAX),
     +                 YSAVE(NEQMAX,NY2DIM)
      INTEGER          IA(NIA), INFORM(NINF), JA(NJA), JACPVT(NJCPVT)
      LOGICAL          ALGEQU(NEQMAX), LDERIV(2)
*     .. External Subroutines ..
      EXTERNAL         D02NJF, D02NUF, D02NVF, D02NXF, D02NYF, JAC,
     +                 MONITR, RESID, X04ABF
*     .. Data statements ..
      DATA             IA/1, 3, 6, 9/, JA/1, 2, 1, 2, 3, 1, 2, 3/
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D02NJF Example Program Results'
      CALL X04ABF(1,NOUT)
*
*     First case. Integrate to TOUT by overshooting (ITASK=1) using
*     B.D.F formulae with a Newton method. Also set PETZLD to
*     .TRUE. so that the Petzold error test is used (since an algebraic
*     equation is defined in the system). Default values for the
*     array CONST are used. Employ vector relative tolerance and scalar
*     absolute tolerance. The Jacobian is supplied by JAC and its
*     structure is determined internally by calls to JAC.
*     The MONITR routine is used to force a return when the first
*     component of the system falls below the value 0.9.
*
      T = 0.0D0
      TOUT = 10.0D0
      ITASK = 1
      Y(1) = 1.0D0
      Y(2) = 0.0D0
      Y(3) = 0.0D0
      LDERIV(1) = .FALSE.
      LDERIV(2) = .FALSE.
      ITOL = 3
      RTOL(1) = 1.0D-4
      RTOL(2) = 1.0D-3
      RTOL(3) = 1.0D-4
      ATOL(1) = 1.0D-7
      DO 20 I = 1, 6
         CONST(I) = 0.0D0
   20 CONTINUE
      ISPLIT = 0
      IFAIL = 0
*
      CALL D02NVF(NEQMAX,NY2DIM,MAXORD,'Newton',PETZLD,CONST,TCRIT,HMIN,
     +            HMAX,H0,MAXSTP,MXHNIL,'Average-L2',RWORK,IFAIL)
      CALL D02NUF(NEQ,NEQMAX,'Analytical',NWKJAC,IA,NIA,JA,NJA,JACPVT,
     +            NJCPVT,SENS,U,ETA,LBLOCK,ISPLIT,RWORK,IFAIL)
*
      WRITE (NOUT,*)
      WRITE (NOUT,*) '  Analytic Jacobian, structure not supplied'
      WRITE (NOUT,*)
      WRITE (NOUT,*) '    X          Y(1)           Y(2)           Y(3)'
      WRITE (NOUT,99999) T, (Y(I),I=1,NEQ)
*
*     Soft fail and error messages only
      ITRACE = 0
      IFAIL = 1
*
      CALL D02NJF(NEQ,NEQMAX,T,TOUT,Y,YDOT,RWORK,RTOL,ATOL,ITOL,INFORM,
     +            RESID,YSAVE,NY2DIM,JAC,WKJAC,NWKJAC,JACPVT,NJCPVT,
     +            MONITR,LDERIV,ITASK,ITRACE,IFAIL)
*
      IF (IFAIL.EQ.0 .OR. IFAIL.EQ.12) THEN
         WRITE (NOUT,99999) T, (Y(I),I=1,NEQ)
         IFAIL = 0
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
         WRITE (NOUT,99995) ' Max err comp = ', IMXER
         ICALL = 0
*
         CALL D02NXF(ICALL,LIWREQ,LIWUSD,LRWREQ,LRWUSD,NLU,NNZ,NGP,
     +               ISPLIT,IGROW,LBLOCK,NBLOCK,INFORM)
*
         WRITE (NOUT,*)
         WRITE (NOUT,99994) ' NJCPVT (required ', LIWREQ, '  used ',
     +     LIWUSD, ')'
         WRITE (NOUT,99994) ' NWKJAC (required ', LRWREQ, '  used ',
     +     LRWUSD, ')'
         WRITE (NOUT,99993) ' No. of LU-decomps ', NLU,
     +     '  No. of nonzeros ', NNZ
         WRITE (NOUT,99992) ' No. of FCN calls to form Jacobian ', NGP,
     +     '  Try ISPLIT ', ISPLIT
         WRITE (NOUT,99991) ' Growth est ', IGROW,
     +     '  No. of blocks on diagonal ', NBLOCK
      ELSE IF (IFAIL.EQ.10) THEN
         ICALL = 1
*
         CALL D02NXF(ICALL,LIWREQ,LIWUSD,LRWREQ,LRWUSD,NLU,NNZ,NGP,
     +               ISPLIT,IGROW,LBLOCK,NBLOCK,INFORM)
*
         WRITE (NOUT,*)
         WRITE (NOUT,99994) ' NJCPVT (required ', LIWREQ, '  used ',
     +     LIWUSD, ')'
         WRITE (NOUT,99994) ' NWKJAC (required ', LRWREQ, '  used ',
     +     LRWUSD, ')'
      ELSE
         WRITE (NOUT,*)
         WRITE (NOUT,99998) 'Exit D02NJF with IFAIL = ', IFAIL,
     +     '  and T = ', T
      END IF
*
*     Second case. Integrate to TOUT by overshooting (ITASK=1) using
*     B.D.F formulae with a Newton method. Also set PETZLD to
*     .TRUE. so that the Petzold error test is used (since an algebraic
*     equation is defined in the system). Default values for the
*     array CONST are used. Employ vector relative tolerance and scalar
*     absolute tolerance. The Jacobian is supplied by JAC and its
*     structure is also supplied.
*     The MONITR routine is used to force a return when the first
*     component of the system falls below the value 0.9.
*
      T = 0.0D0
      Y(1) = 1.0D0
      Y(2) = 0.0D0
      Y(3) = 0.0D0
*
      ISPLIT = 0
      IFAIL = 0
*
      CALL D02NVF(NEQMAX,NY2DIM,MAXORD,'Newton',PETZLD,CONST,TCRIT,HMIN,
     +            HMAX,H0,MAXSTP,MXHNIL,'Average-L2',RWORK,IFAIL)
*
      CALL D02NUF(NEQ,NEQMAX,'Full information',NWKJAC,IA,NIA,JA,NJA,
     +            JACPVT,NJCPVT,SENS,U,ETA,LBLOCK,ISPLIT,RWORK,IFAIL)
*
      LDERIV(1) = .FALSE.
      LDERIV(2) = .FALSE.
*
      WRITE (NOUT,*)
      WRITE (NOUT,*) '  Analytic Jacobian, structure supplied'
      WRITE (NOUT,*)
      WRITE (NOUT,*) '    X          Y(1)           Y(2)           Y(3)'
      WRITE (NOUT,99999) T, (Y(I),I=1,NEQ)
      IFAIL = 1
*
      CALL D02NJF(NEQ,NEQMAX,T,TOUT,Y,YDOT,RWORK,RTOL,ATOL,ITOL,INFORM,
     +            RESID,YSAVE,NY2DIM,JAC,WKJAC,NWKJAC,JACPVT,NJCPVT,
     +            MONITR,LDERIV,ITASK,ITRACE,IFAIL)
*
      IF (IFAIL.EQ.0 .OR. IFAIL.EQ.12) THEN
         WRITE (NOUT,99999) T, (Y(I),I=1,NEQ)
         IFAIL = 0
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
         WRITE (NOUT,99995) ' Max err comp = ', IMXER
         WRITE (NOUT,*)
         ICALL = 0
*
         CALL D02NXF(ICALL,LIWREQ,LIWUSD,LRWREQ,LRWUSD,NLU,NNZ,NGP,
     +               ISPLIT,IGROW,LBLOCK,NBLOCK,INFORM)
*
         WRITE (NOUT,*)
         WRITE (NOUT,99994) ' NJCPVT (required ', LIWREQ, '  used ',
     +     LIWUSD, ')'
         WRITE (NOUT,99994) ' NWKJAC (required ', LRWREQ, '  used ',
     +     LRWUSD, ')'
         WRITE (NOUT,99993) ' No. of LU-decomps ', NLU,
     +     '  No. of nonzeros ', NNZ
         WRITE (NOUT,99992) ' No. of FCN calls to form Jacobian ', NGP,
     +     '  Try ISPLIT ', ISPLIT
         WRITE (NOUT,99991) ' Growth est ', IGROW,
     +     '  No. of blocks on diagonal ', NBLOCK
      ELSE IF (IFAIL.EQ.10) THEN
         ICALL = 1
*
         CALL D02NXF(ICALL,LIWREQ,LIWUSD,LRWREQ,LRWUSD,NLU,NNZ,NGP,
     +               ISPLIT,IGROW,LBLOCK,NBLOCK,INFORM)
*
         WRITE (NOUT,*)
         WRITE (NOUT,99994) ' NJCPVT (required ', LIWREQ, '  used ',
     +     LIWUSD, ')'
         WRITE (NOUT,99994) ' NWKJAC (required ', LRWREQ, '  used ',
     +     LRWUSD, ')'
      ELSE
         WRITE (NOUT,*)
         WRITE (NOUT,99998) 'Exit D02NJF with IFAIL = ', IFAIL,
     +     '  and T = ', T
      END IF
      STOP
*
99999 FORMAT (1X,F8.3,3(F13.5,2X))
99998 FORMAT (1X,A,I2,A,D12.5)
99997 FORMAT (1X,A,D12.5,A,D12.5,A,D12.5)
99996 FORMAT (1X,A,I6,A,I6,A,I6)
99995 FORMAT (1X,A,I4)
99994 FORMAT (1X,A,I8,A,I8,A)
99993 FORMAT (1X,A,I4,A,I8)
99992 FORMAT (1X,A,I4,A,I4)
99991 FORMAT (1X,A,I8,A,I4)
      END
*
      SUBROUTINE RESID(NEQ,T,Y,YDOT,R,IRES)
*     .. Scalar Arguments ..
      DOUBLE PRECISION T
      INTEGER          IRES, NEQ
*     .. Array Arguments ..
      DOUBLE PRECISION R(NEQ), Y(NEQ), YDOT(NEQ)
*     .. Executable Statements ..
      R(1) = 0.0D0
      R(2) = -YDOT(2)
      R(3) = -YDOT(3)
      IF (IRES.EQ.1) THEN
         R(1) = Y(1) + Y(2) + Y(3) - 1.0D0 + R(1)
         R(2) = 0.04D0*Y(1) - 1.0D4*Y(2)*Y(3) - 3.0D7*Y(2)*Y(2) + R(2)
         R(3) = 3.0D7*Y(2)*Y(2) + R(3)
      END IF
      RETURN
      END
*
      SUBROUTINE JAC(NEQ,T,Y,YDOT,H,D,J,PDJ)
*     .. Scalar Arguments ..
      DOUBLE PRECISION D, H, T
      INTEGER        J, NEQ
*     .. Array Arguments ..
      DOUBLE PRECISION PDJ(NEQ), Y(NEQ), YDOT(NEQ)
*     .. Local Scalars ..
      DOUBLE PRECISION HXD
*     .. Executable Statements ..
      HXD = H*D
      IF (J.EQ.1) THEN
         PDJ(1) = 0.0D0 - HXD*(1.0D0)
         PDJ(2) = 0.0D0 - HXD*(0.04D0)
*        PDJ(3) = 0.0 - HXD*(0.)
      ELSE IF (J.EQ.2) THEN
         PDJ(1) = 0.0D0 - HXD*(1.0D0)
         PDJ(2) = 1.0D0 - HXD*(-1.0D4*Y(3)-6.0D7*Y(2))
         PDJ(3) = 0.0D0 - HXD*(6.0D7*Y(2))
      ELSE IF (J.EQ.3) THEN
         PDJ(1) = 0.0D0 - HXD*(1.0D0)
         PDJ(2) = 0.0D0 - HXD*(-1.0D4*Y(2))
         PDJ(3) = 1.0D0 - HXD*(0.0D0)
      END IF
      RETURN
      END
*
      SUBROUTINE MONITR(N,NMAX,T,HLAST,H,Y,YDOT,YSAVE,R,ACOR,IMON,INLN,
     +                  HMIN,HMXI,NQU)
*     .. Scalar Arguments ..
      DOUBLE PRECISION  H, HLAST, HMIN, HMXI, T
      INTEGER           IMON, INLN, N, NMAX, NQU
*     .. Array Arguments ..
      DOUBLE PRECISION  ACOR(NMAX,2), R(N), Y(N), YDOT(N), YSAVE(NMAX,*)
*     .. Executable Statements ..
      IF (Y(1).LE.0.9D0) IMON = -2
      RETURN
      END
