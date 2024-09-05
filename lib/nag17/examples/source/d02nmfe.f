*     D02NMF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          NEQ, NEQMAX, NRW, NINF, NWKJAC, NJCPVT, MAXORD,
     +                 NY2DIM, MAXSTP, MXHNIL
      PARAMETER        (NEQ=3,NEQMAX=NEQ,NRW=50+4*NEQMAX,NINF=23,
     +                 NWKJAC=NEQMAX*(NEQMAX+1),NJCPVT=1,MAXORD=5,
     +                 NY2DIM=MAXORD+1,MAXSTP=200,MXHNIL=5)
      INTEGER          LACORB, LSAVRB
      PARAMETER        (LACORB=50+NEQMAX,LSAVRB=LACORB+NEQMAX)
      DOUBLE PRECISION H0, HMAX, HMIN, TCRIT
      PARAMETER        (H0=0.0D0,HMAX=10.0D0,HMIN=1.0D-10,TCRIT=0.0D0)
      LOGICAL          PETZLD
      PARAMETER        (PETZLD=.FALSE.)
*     .. Local Scalars ..
      DOUBLE PRECISION H, HLAST, HNEXT, HU, T, TC, TCUR, TOLSF, TOUT,
     +                 XOUT
      INTEGER          I, IFAIL, IFLAG, IMON, IMXER, INLN, IOUT, IRES,
     +                 IREVCM, ITASK, ITOL, ITRACE, LACOR1, LACOR2,
     +                 LACOR3, LSAVR1, LSAVR2, LSAVR3, NITER, NJE, NQ,
     +                 NQU, NRE, NST
*     .. Local Arrays ..
      DOUBLE PRECISION ATOL(NEQMAX), CONST(6), RTOL(NEQMAX), RWORK(NRW),
     +                 WKJAC(NWKJAC), Y(NEQMAX), YDOT(NEQMAX),
     +                 YSAVE(NEQMAX,NY2DIM)
      INTEGER          INFORM(NINF), JACPVT(NJCPVT)
      LOGICAL          ALGEQU(NEQMAX)
*     .. External Subroutines ..
      EXTERNAL         D02NMF, D02NSF, D02NVF, D02NYF, D02XKF, X04ABF
*     .. Intrinsic Functions ..
      INTRINSIC        INT, DBLE
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D02NMF Example Program Results'
      WRITE (NOUT,*)
      CALL X04ABF(1,NOUT)
*
*     Integrate to TOUT by overshooting TOUT (ITASK=1) using B.D.F.
*     formulae with a Newton method. Default values for the array CONST
*     are used. Employ scalar tolerances and the Jacobian is evaluated
*     internally. On the reverse communication call equivalent to the
*     MONITR call in forward communication routines carry out
*     interpolation using D02XKF.
*
      T = 0.0D0
      TOUT = 10.0D0
      ITASK = 1
      IOUT = 1
      XOUT = 2.0D0
      Y(1) = 1.0D0
      Y(2) = 0.0D0
      Y(3) = 0.0D0
      ITOL = 1
      RTOL(1) = 1.0D-4
      ATOL(1) = 1.0D-7
      DO 20 I = 1, 6
         CONST(I) = 0.0D0
   20 CONTINUE
      IFAIL = 0
*
      CALL D02NVF(NEQMAX,NY2DIM,MAXORD,'Newton',PETZLD,CONST,TCRIT,HMIN,
     +            HMAX,H0,MAXSTP,MXHNIL,'Average-L2',RWORK,IFAIL)
      CALL D02NSF(NEQ,NEQMAX,'Numerical',NWKJAC,RWORK,IFAIL)
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
*     Soft fail and error messages only
      IREVCM = 0
      ITRACE = 0
   40 IFAIL = 1
*
      CALL D02NMF(NEQ,NEQMAX,T,TOUT,Y,YDOT,RWORK,RTOL,ATOL,ITOL,INFORM,
     +            YSAVE,NY2DIM,WKJAC,NWKJAC,JACPVT,NJCPVT,IMON,INLN,
     +            IRES,IREVCM,ITASK,ITRACE,IFAIL)
*
      IF (IREVCM.NE.0) THEN
         IF (IREVCM.EQ.1 .OR. IREVCM.EQ.3) THEN
*           Equivalent to FCN evaluation in forward communication
*           routines
            RWORK(LSAVR1) = -0.04D0*Y(1) + 1.0D4*Y(2)*Y(3)
            RWORK(LSAVR2) = 0.04D0*Y(1) - 1.0D4*Y(2)*Y(3) - 3.0D7*Y(2)
     +                      *Y(2)
            RWORK(LSAVR3) = 3.0D7*Y(2)*Y(2)
         ELSE IF (IREVCM.EQ.4) THEN
*           Equivalent to FCN evaluation in forward communication
*           routines
            RWORK(LACOR1) = -0.04D0*Y(1) + 1.0D4*Y(2)*Y(3)
            RWORK(LACOR2) = 0.04D0*Y(1) - 1.0D4*Y(2)*Y(3) - 3.0D7*Y(2)
     +                      *Y(2)
            RWORK(LACOR3) = 3.0D7*Y(2)*Y(2)
         ELSE IF (IREVCM.EQ.5) THEN
*           Equivalent to FCN evaluation in forward communication
*           routines
            YDOT(1) = -0.04D0*Y(1) + 1.0D4*Y(2)*Y(3)
            YDOT(2) = 0.04D0*Y(1) - 1.0D4*Y(2)*Y(3) - 3.0D7*Y(2)*Y(2)
            YDOT(3) = 3.0D7*Y(2)*Y(2)
         ELSE IF (IREVCM.EQ.9) THEN
*           Equivalent to MONITR call in forward communication routines
            IF (IMON.EQ.1) THEN
               TC = RWORK(19)
               HLAST = RWORK(15)
               HNEXT = RWORK(16)
               NQU = INT(RWORK(10))
   60          CONTINUE
               IF (TC-HLAST.LT.XOUT .AND. XOUT.LE.TC) THEN
                  IFLAG = 1
*
                  CALL D02XKF(XOUT,RWORK(LSAVR1),NEQ,YSAVE,NEQMAX,
     +                        NY2DIM,RWORK(LACOR1),NEQ,TC,NQU,HLAST,
     +                        HNEXT,IFLAG)
*
                  IF (IFLAG.NE.0) THEN
                     IMON = -2
                  ELSE
                     WRITE (NOUT,99999) XOUT, (RWORK(LSAVRB+I),I=1,NEQ)
                     IOUT = IOUT + 1
                     XOUT = DBLE(IOUT)*2.0D0
                     IF (IOUT.LT.6) GO TO 60
                  END IF
               END IF
            END IF
         ELSE IF (IREVCM.EQ.2 .OR. IREVCM.EQ.6 .OR. IREVCM.EQ.7 .OR.
     +            IREVCM.EQ.8) THEN
            WRITE (NOUT,*)
            WRITE (NOUT,99995) 'Illegal value of IREVCM = ', IREVCM
            STOP
         END IF
         GO TO 40
      ELSE
         IF (IFAIL.EQ.0) THEN
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
            WRITE (NOUT,99995) ' Max err comp = ', IMXER
            WRITE (NOUT,*)
         ELSE
            WRITE (NOUT,*)
            WRITE (NOUT,99998) 'Exit D02NMF with IFAIL = ', IFAIL,
     +        '  and T = ', T
         END IF
      END IF
      STOP
*
99999 FORMAT (1X,F8.3,3(F13.5,2X))
99998 FORMAT (1X,A,I2,A,D12.5)
99997 FORMAT (1X,A,D12.5,A,D12.5,A,D12.5)
99996 FORMAT (1X,A,I6,A,I6,A,I6)
99995 FORMAT (1X,A,I4)
      END
