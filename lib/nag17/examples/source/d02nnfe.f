*     D02NNF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          NEQ, NEQMAX, NRW, NINF, NJCPVT, NWKJAC, NIA, NJA,
     +                 MAXORD, NY2DIM, MAXSTP, MXHNIL
      PARAMETER        (NEQ=3,NEQMAX=NEQ,NRW=50+4*NEQMAX,NINF=23,
     +                 NJCPVT=150,NWKJAC=100,NIA=1,NJA=1,MAXORD=5,
     +                 NY2DIM=MAXORD+1,MAXSTP=200,MXHNIL=5)
      INTEGER          LACORB, LSAVRB
      PARAMETER        (LACORB=50+NEQMAX,LSAVRB=LACORB+NEQMAX)
      DOUBLE PRECISION H0, HMAX, HMIN, TCRIT
      PARAMETER        (H0=1.0D-4,HMAX=10.0D0,HMIN=1.0D-10,TCRIT=0.0D0)
      LOGICAL          PETZLD
      PARAMETER        (PETZLD=.TRUE.)
      DOUBLE PRECISION ETA, U, SENS
      PARAMETER        (ETA=1.0D-4,U=0.1D0,SENS=1.0D-6)
      LOGICAL          LBLOCK
      PARAMETER        (LBLOCK=.TRUE.)
*     .. Local Scalars ..
      DOUBLE PRECISION H, HU, HXD, T, TCUR, TOLSF, TOUT
      INTEGER          I, ICALL, IFAIL, IGROW, IMON, IMXER, INLN,
     +                 IPLACE, IRES, IREVCM, ISPLIT, ITASK, ITOL,
     +                 ITRACE, J, LACOR1, LACOR2, LACOR3, LIWREQ,
     +                 LIWUSD, LRWREQ, LRWUSD, LSAVR1, LSAVR2, LSAVR3,
     +                 NBLOCK, NFAILS, NGP, NITER, NJE, NLU, NNZ, NQ,
     +                 NQU, NRE, NST
*     .. Local Arrays ..
      DOUBLE PRECISION ATOL(NEQMAX), CONST(6), RTOL(NEQMAX), RWORK(NRW),
     +                 WKJAC(NWKJAC), Y(NEQMAX), YDOT(NEQMAX),
     +                 YSAVE(NEQMAX,NY2DIM)
      INTEGER          IA(NIA), INFORM(NINF), JA(NJA), JACPVT(NJCPVT)
      LOGICAL          ALGEQU(NEQMAX), LDERIV(2)
*     .. External Subroutines ..
      EXTERNAL         D02NNF, D02NRF, D02NUF, D02NVF, D02NXF, D02NYF,
     +                 X04ABF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D02NNF Example Program Results'
      WRITE (NOUT,*)
      CALL X04ABF(1,NOUT)
*
*     Integrate towards TOUT stopping at the first mesh point beyond
*     TOUT (ITASK=3) using the B.D.F. formulae with a Newton method.
*     Employ scalar tolerances and the Jacobian is supplied, but its
*     structure is evaluated internally by calls to the Jacobian
*     forming part of the program (IREVCM=8). Default values for the
*     array CONST are used. Also count the number of step failures
*     (IREVCM=10).
*
      T = 0.0D0
      TOUT = 10.0D0
      ITASK = 3
      Y(1) = 1.0D0
      Y(2) = 0.0D0
      Y(3) = 0.0D0
      LDERIV(1) = .FALSE.
      LDERIV(2) = .FALSE.
      ITOL = 1
      RTOL(1) = 1.0D-4
      ATOL(1) = 1.0D-7
      DO 20 I = 1, 6
         CONST(I) = 0.0D0
   20 CONTINUE
      ISPLIT = 0
      NFAILS = 0
      IFAIL = 0
*
      CALL D02NVF(NEQMAX,NY2DIM,MAXORD,'Newton',PETZLD,CONST,TCRIT,HMIN,
     +            HMAX,H0,MAXSTP,MXHNIL,'Average-l2',RWORK,IFAIL)
      CALL D02NUF(NEQ,NEQMAX,'Analytical',NWKJAC,IA,NIA,JA,NJA,JACPVT,
     +            NJCPVT,SENS,U,ETA,LBLOCK,ISPLIT,RWORK,IFAIL)
*
*     Soft fail and error messages only
      IREVCM = 0
      IFAIL = 1
      ITRACE = 0
*
      LACOR1 = LACORB + 1
      LACOR2 = LACORB + 2
      LACOR3 = LACORB + 3
      LSAVR1 = LSAVRB + 1
      LSAVR2 = LSAVRB + 2
      LSAVR3 = LSAVRB + 3
      WRITE (NOUT,*) '    X          Y(1)           Y(2)           Y(3)'
      WRITE (NOUT,99999) T, (Y(I),I=1,NEQ)
*
   40 CONTINUE
*
      CALL D02NNF(NEQ,NEQMAX,T,TOUT,Y,YDOT,RWORK,RTOL,ATOL,ITOL,INFORM,
     +            YSAVE,NY2DIM,WKJAC,NWKJAC,JACPVT,NJCPVT,IMON,INLN,
     +            IRES,IREVCM,LDERIV,ITASK,ITRACE,IFAIL)
*
      IF (IREVCM.GT.0) THEN
         IF (IREVCM.EQ.1 .OR. IREVCM.EQ.3 .OR. IREVCM.EQ.6 .OR.
     +       IREVCM.EQ.11) THEN
*           Equivalent to RESID evaluation in forward communication
*           routines
            RWORK(LSAVR1) = -YDOT(1) - YDOT(2) - YDOT(3)
            RWORK(LSAVR2) = -YDOT(2)
            RWORK(LSAVR3) = -YDOT(3)
            IF (IRES.EQ.1) THEN
               RWORK(LSAVR1) = 0.0D0 + RWORK(LSAVR1)
               RWORK(LSAVR2) = 0.04D0*Y(1) - 1.0D4*Y(2)*Y(3) -
     +                         3.0D7*Y(2)*Y(2) + RWORK(LSAVR2)
               RWORK(LSAVR3) = 3.0D7*Y(2)*Y(2) + RWORK(LSAVR3)
            END IF
         ELSE IF (IREVCM.EQ.2) THEN
*           Equivalent to RESID evaluation in forward communication
*           routines
            RWORK(LSAVR1) = -RWORK(LACOR1) - RWORK(LACOR2) -
     +                      RWORK(LACOR3)
            RWORK(LSAVR2) = -RWORK(LACOR2)
            RWORK(LSAVR3) = -RWORK(LACOR3)
         ELSE IF (IREVCM.EQ.4 .OR. IREVCM.EQ.7) THEN
*           Equivalent to RESID evaluation in forward communication
*           routines
            RWORK(LACOR1) = -YDOT(1) - YDOT(2) - YDOT(3)
            RWORK(LACOR2) = -YDOT(2)
            RWORK(LACOR3) = -YDOT(3)
            IF (IRES.EQ.1) THEN
               RWORK(LACOR1) = 0.0D0 + RWORK(LACOR1)
               RWORK(LACOR2) = 0.04D0*Y(1) - 1.0D4*Y(2)*Y(3) -
     +                         3.0D7*Y(2)*Y(2) + RWORK(LACOR2)
               RWORK(LACOR3) = 3.0D7*Y(2)*Y(2) + RWORK(LACOR3)
            END IF
         ELSE IF (IREVCM.EQ.5) THEN
*           Equivalent to RESID evaluation in forward communication
*           routines
            YDOT(1) = 0.0D0 - RWORK(LSAVR1) - RWORK(LSAVR2) -
     +                RWORK(LSAVR3)
            YDOT(2) = 0.04D0*Y(1) - 1.0D4*Y(2)*Y(3) - 3.0D7*Y(2)*(2) -
     +                RWORK(LSAVR2)
            YDOT(3) = 3.0D7*Y(2)*Y(2) - RWORK(LSAVR3)
         ELSE IF (IREVCM.EQ.8) THEN
*           Equivalent to JAC evaluation in forward communication
*           routines
            CALL D02NRF(J,IPLACE,INFORM)
*
            HXD = RWORK(16)*RWORK(20)
*
            IF (IPLACE.LT.2) THEN
               IF (J.LT.2) THEN
                  RWORK(LSAVR1) = 1.0D0 - HXD*(0.0D0)
                  RWORK(LSAVR2) = 0.0D0 - HXD*(0.04D0)
*                 RWORK(LSAVR3) = 0.0 - HXD*(0.0)
               ELSE IF (J.EQ.2) THEN
                  RWORK(LSAVR1) = 1.0D0 - HXD*(0.0D0)
                  RWORK(LSAVR2) = 1.0D0 - HXD*(-1.0D4*Y(3)-6.0D7*Y(2))
                  RWORK(LSAVR3) = 0.0D0 - HXD*(6.0D7*Y(2))
               ELSE IF (J.GT.2) THEN
                  RWORK(LSAVR1) = 1.0D0 - HXD*(0.0D0)
                  RWORK(LSAVR2) = 0.0D0 - HXD*(-1.0D4*Y(2))
                  RWORK(LSAVR3) = 1.0D0 - HXD*(0.0D0)
               END IF
            ELSE
               IF (J.LT.2) THEN
                  RWORK(LACOR1) = 1.0D0 - HXD*(0.0D0)
                  RWORK(LACOR2) = 0.0D0 - HXD*(0.04D0)
*                 RWORK(LACOR3) = 0.0 - HXD*(0.0)
               ELSE IF (J.EQ.2) THEN
                  RWORK(LACOR1) = 1.0D0 - HXD*(0.0D0)
                  RWORK(LACOR2) = 1.0D0 - HXD*(-1.0D4*Y(3)-6.0D7*Y(2))
                  RWORK(LACOR3) = 0.0D0 - HXD*(6.0D7*Y(2))
               ELSE IF (J.GT.2) THEN
                  RWORK(LACOR1) = 1.0D0 - HXD*(0.0D0)
                  RWORK(LACOR2) = 0.0D0 - HXD*(-1.0D4*Y(2))
                  RWORK(LACOR3) = 1.0D0 - HXD*(0.0D0)
               END IF
            END IF
*           Step failure
         ELSE IF (IREVCM.EQ.10) THEN
            NFAILS = NFAILS + 1
         END IF
         GO TO 40
      ELSE
         IF (IFAIL.EQ.0) THEN
            WRITE (NOUT,99999) T, (Y(I),I=1,NEQ)
*
            CALL D02NYF(NEQ,NEQMAX,HU,H,TCUR,TOLSF,RWORK,NST,NRE,NJE,
     +                  NQU,NQ,NITER,IMXER,ALGEQU,INFORM,IFAIL)
*
            WRITE (NOUT,*)
            WRITE (NOUT,99997) ' HUSED = ', HU, '  HNEXT = ', H,
     +        '  TCUR = ', TCUR
            WRITE (NOUT,99996) ' NST = ', NST, '    NRE = ', NRE,
     +        '    NJE = ', NJE
            WRITE (NOUT,99996) ' NQU = ', NQU, '    NQ  = ', NQ,
     +        '  NITER = ', NITER
            WRITE (NOUT,99995) ' Max err comp = ', IMXER,
     +        '   No. of failed steps = ', NFAILS
            ICALL = 0
*
            CALL D02NXF(ICALL,LIWREQ,LIWUSD,LRWREQ,LRWUSD,NLU,NNZ,NGP,
     +                  ISPLIT,IGROW,LBLOCK,NBLOCK,INFORM)
*
            WRITE (NOUT,*)
            WRITE (NOUT,99994) ' NJCPVT (required ', LIWREQ, '  used ',
     +        LIWUSD, ')'
            WRITE (NOUT,99994) ' NWKJAC (required ', LRWREQ, '  used ',
     +        LRWUSD, ')'
            WRITE (NOUT,99993) ' No. of LU-decomps ', NLU,
     +        '  No. of nonzeros ', NNZ
            WRITE (NOUT,99995) ' No. of FCN calls to form Jacobian ',
     +        NGP, '  Try ISPLIT ', ISPLIT
            WRITE (NOUT,99992) ' Growth est ', IGROW,
     +        '  No. of blocks on diagonal ', NBLOCK
         ELSE IF (IFAIL.EQ.10) THEN
            ICALL = 1
*
            CALL D02NXF(ICALL,LIWREQ,LIWUSD,LRWREQ,LRWUSD,NLU,NNZ,NGP,
     +                  ISPLIT,IGROW,LBLOCK,NBLOCK,INFORM)
*
            WRITE (NOUT,*)
            WRITE (NOUT,99994) ' NJCPVT (required ', LIWREQ, '  used ',
     +        LIWUSD, ')'
            WRITE (NOUT,99994) ' NWKJAC (required ', LRWREQ, '  used ',
     +        LRWUSD, ')'
         ELSE
            WRITE (NOUT,*)
            WRITE (NOUT,99998) 'Exit D02NNF with IFAIL = ', IFAIL,
     +        '  and T = ', T
         END IF
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
