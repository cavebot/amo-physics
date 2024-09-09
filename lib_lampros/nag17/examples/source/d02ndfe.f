*     D02NDF Example Program Text
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
      PARAMETER        (PETZLD=.FALSE.)
      DOUBLE PRECISION ETA, U, SENS
      PARAMETER        (ETA=1.0D-4,U=0.1D0,SENS=0.0D0)
      LOGICAL          LBLOCK
      PARAMETER        (LBLOCK=.TRUE.)
*     .. Scalars in Common ..
      DOUBLE PRECISION XOUT
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
      LOGICAL          ALGEQU(NEQMAX)
*     .. External Subroutines ..
      EXTERNAL         D02NDF, D02NDZ, D02NUF, D02NVF, D02NXF, D02NYF,
     +                 FCN, MONITR, X04ABF
*     .. Common blocks ..
      COMMON           XOUT
*     .. Data statements ..
      DATA             IA/1, 3, 6, 9/, JA/1, 2, 1, 2, 3, 1, 2, 3/
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D02NDF Example Program Results'
      CALL X04ABF(1,NOUT)
*
*     First case. Integrate to TOUT by overshooting (ITASK=1) using
*     using B.D.F formulae with a Newton method. Default values for the
*     array CONST are used. Employ scalar relative tolerance and scalar
*     absolute tolerance. The Jacobian and its structure are evaluated
*     internally. Carry out interpolation in the MONITR routine using
*     the NAG routine D02XKF.
*
      T = 0.0D0
      TOUT = 10.0D0
      ITASK = 1
      Y(1) = 1.0D0
      Y(2) = 0.0D0
      Y(3) = 0.0D0
      ITOL = 1
      RTOL(1) = 1.0D-4
      ATOL(1) = 1.0D-7
      DO 20 I = 1, 6
         CONST(I) = 0.0D0
   20 CONTINUE
      ISPLIT = 0
      IFAIL = 0
*
      CALL D02NVF(NEQMAX,NY2DIM,MAXORD,'Newton',PETZLD,CONST,TCRIT,HMIN,
     +            HMAX,H0,MAXSTP,MXHNIL,'Average-L2',RWORK,IFAIL)
*
      CALL D02NUF(NEQ,NEQMAX,'Numerical',NWKJAC,IA,NIA,JA,NJA,JACPVT,
     +            NJCPVT,SENS,U,ETA,LBLOCK,ISPLIT,RWORK,IFAIL)
*
      WRITE (NOUT,*)
      WRITE (NOUT,*) '  Numerical Jacobian, structure not supplied'
      WRITE (NOUT,*)
      WRITE (NOUT,*) '    X          Y(1)           Y(2)           Y(3)'
      WRITE (NOUT,99999) T, (Y(I),I=1,NEQ)
      XOUT = 2.0D0
*
*     Soft fail and error messages only
      ITRACE = 0
      IFAIL = 1
*
      CALL D02NDF(NEQ,NEQMAX,T,TOUT,Y,YDOT,RWORK,RTOL,ATOL,ITOL,INFORM,
     +            FCN,YSAVE,NY2DIM,D02NDZ,WKJAC,NWKJAC,JACPVT,NJCPVT,
     +            MONITR,ITASK,ITRACE,IFAIL)
*
      IF (IFAIL.EQ.0) THEN
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
         WRITE (NOUT,99995) ' No. of FCN calls to form Jacobian ', NGP,
     +     '  Try ISPLIT ', ISPLIT
         WRITE (NOUT,99992) ' Growth est ', IGROW,
     +     '  No. of blocks on diagonal ', NBLOCK
      ELSE IF (IFAIL.EQ.10) THEN
         WRITE (NOUT,*)
         WRITE (NOUT,99998) 'Exit D02NDF with IFAIL = ', IFAIL,
     +     '  and T = ', T
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
         WRITE (NOUT,99998) 'Exit D02NDF with IFAIL = ', IFAIL,
     +     '  and T = ', T
      END IF
*
*     Second case. Integrate to TOUT by overshooting (ITASK=1) using
*     B.D.F formulae with a Newton method. Default values for the
*     array CONST are used. Employ scalar relative tolerance and scalar
*     absolute tolerance. The Jacobian is evaluated internally but its
*     structure is supplied. Carry out interpolation in the
*     MONITR routine using the NAG routine D02XKF.
*
      T = 0.0D0
      Y(1) = 1.0D0
      Y(2) = 0.0D0
      Y(3) = 0.0D0
      ISPLIT = 0
      IFAIL = 0
*
      CALL D02NVF(NEQMAX,NY2DIM,MAXORD,'Newton',PETZLD,CONST,TCRIT,HMIN,
     +            HMAX,H0,MAXSTP,MXHNIL,'Average-L2',RWORK,IFAIL)
      CALL D02NUF(NEQ,NEQMAX,'Structural',NWKJAC,IA,NIA,JA,NJA,JACPVT,
     +            NJCPVT,SENS,U,ETA,LBLOCK,ISPLIT,RWORK,IFAIL)
*
      WRITE (NOUT,*)
      WRITE (NOUT,*) '  Numerical Jacobian, structure supplied'
      WRITE (NOUT,*)
      WRITE (NOUT,*) '    X          Y(1)           Y(2)           Y(3)'
      WRITE (NOUT,99999) T, (Y(I),I=1,NEQ)
      XOUT = 2.0D0
*
*     Soft fail and error messages only
      ITRACE = 0
      IFAIL = 1
*
      CALL D02NDF(NEQ,NEQMAX,T,TOUT,Y,YDOT,RWORK,RTOL,ATOL,ITOL,INFORM,
     +            FCN,YSAVE,NY2DIM,D02NDZ,WKJAC,NWKJAC,JACPVT,NJCPVT,
     +            MONITR,ITASK,ITRACE,IFAIL)
*
      IF (IFAIL.EQ.0) THEN
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
         WRITE (NOUT,99995) ' No. of FCN calls to form Jacobian ', NGP,
     +     '  Try ISPLIT ', ISPLIT
         WRITE (NOUT,99992) ' Growth est ', IGROW,
     +     '  No. of blocks on diagonal ', NBLOCK
      ELSE IF (IFAIL.EQ.10) THEN
         WRITE (NOUT,*)
         WRITE (NOUT,99998) 'Exit D02NDF with IFAIL = ', IFAIL,
     +     '  and T = ', T
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
         WRITE (NOUT,99998) 'Exit D02NDF with IFAIL = ', IFAIL,
     +     '  and T = ', T
      END IF
      STOP
*
99999 FORMAT (1X,F8.3,3(F13.5,2X))
99998 FORMAT (1X,A,I2,A,D12.5)
99997 FORMAT (1X,A,D12.5,A,D12.5,A,D12.5)
99996 FORMAT (1X,A,I6,A,I6,A,I6)
99995 FORMAT (1X,A,I4,A,I4)
99994 FORMAT (1X,A,I8,A,I8,A)
99993 FORMAT (1X,A,I4,A,I8)
99992 FORMAT (1X,A,I8,A,I4)
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
      SUBROUTINE MONITR(N,NMAX,T,HLAST,H,Y,YDOT,YSAVE,R,ACOR,IMON,INLN,
     +                  HMIN,HMXI,NQU)
*     .. Parameters ..
      INTEGER           NOUT
      PARAMETER         (NOUT=6)
      INTEGER           NY2DIM
      PARAMETER         (NY2DIM=6)
*     .. Scalar Arguments ..
      DOUBLE PRECISION  H, HLAST, HMIN, HMXI, T
      INTEGER           IMON, INLN, N, NMAX, NQU
*     .. Array Arguments ..
      DOUBLE PRECISION  ACOR(NMAX,2), R(N), Y(N), YDOT(N), YSAVE(NMAX,*)
*     .. Scalars in Common ..
      DOUBLE PRECISION  XOUT
*     .. Local Scalars ..
      INTEGER           I, IFAIL
*     .. External Subroutines ..
      EXTERNAL          D02XKF
*     .. Common blocks ..
      COMMON            XOUT
*     .. Executable Statements ..
      IF (IMON.NE.1) RETURN
   20 IF ( .NOT. (T-HLAST.LT.XOUT .AND. XOUT.LE.T)) RETURN
      IFAIL = 1
*     C1 interpolation
      CALL D02XKF(XOUT,R,N,YSAVE,NMAX,NY2DIM,ACOR(1,2),N,T,NQU,HLAST,H,
     +            IFAIL)
*
      IF (IFAIL.NE.0) THEN
         IMON = -2
      ELSE
         WRITE (NOUT,99999) XOUT, (R(I),I=1,N)
         XOUT = XOUT + 2.0D0
         IF (XOUT.LT.10.0D0) GO TO 20
      END IF
      RETURN
*
99999 FORMAT (1X,F8.3,3(F13.5,2X))
      END
