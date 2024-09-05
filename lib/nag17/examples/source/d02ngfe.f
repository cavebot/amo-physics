*     D02NGF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          NEQ, NEQMAX, NRW, NINF, NWKJAC, MAXORD, NY2DIM,
     +                 MAXSTP, MXHNIL
      PARAMETER        (NEQ=3,NEQMAX=NEQ,NRW=50+4*NEQMAX,NINF=23,
     +                 NWKJAC=NEQMAX*(NEQMAX+1),MAXORD=5,
     +                 NY2DIM=MAXORD+1,MAXSTP=200,MXHNIL=5)
      DOUBLE PRECISION H0, HMAX, HMIN, TCRIT
      PARAMETER        (H0=0.0D0,HMAX=10.0D0,HMIN=1.0D-10,TCRIT=0.0D0)
      LOGICAL          PETZLD
      PARAMETER        (PETZLD=.FALSE.)
*     .. Local Scalars ..
      DOUBLE PRECISION H, HU, T, TCUR, TOLSF, TOUT, XOUT
      INTEGER          I, IFAIL, IMXER, IOUT, ITASK, ITOL, ITRACE,
     +                 NITER, NJE, NQ, NQU, NRE, NST
*     .. Local Arrays ..
      DOUBLE PRECISION ATOL(NEQMAX), CONST(6), RTOL(NEQMAX), RWORK(NRW),
     +                 SOL(NEQMAX), WKJAC(NWKJAC), Y(NEQMAX),
     +                 YDOT(NEQMAX), YSAVE(NEQMAX,NY2DIM)
      INTEGER          INFORM(NINF)
      LOGICAL          ALGEQU(NEQMAX), LDERIV(2)
*     .. External Subroutines ..
      EXTERNAL         D02NBY, D02NGF, D02NGZ, D02NSF, D02NVF, D02NYF,
     +                 D02XJF, RESID, X04ABF
*     .. Intrinsic Functions ..
      INTRINSIC        DBLE
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D02NGF Example Program Results'
      CALL X04ABF(1,NOUT)
*
*     Integrate to TOUT by overshooting TOUT in one step mode (ITASK=2)
*     using B.D.F formulae with a functional iteration method.
*     Default values for the array CONST are used. Employ vector
*     tolerances and the Jacobian is evaluated internally, if necessary.
*     MONITR subroutine replaced by NAG dummy routine D02NBY.
*     Interpolation outside D02NGF using D02XJF.
*
      T = 0.0D0
      TOUT = 0.1D0
      ITASK = 2
      Y(1) = 1.0D0
      Y(2) = 0.0D0
      Y(3) = 0.0D0
      LDERIV(1) = .FALSE.
      LDERIV(2) = .FALSE.
      ITOL = 4
      RTOL(1) = 1.0D-4
      RTOL(2) = 1.0D-3
      RTOL(3) = 1.0D-4
      ATOL(1) = 1.0D-7
      ATOL(2) = 1.0D-8
      ATOL(3) = 1.0D-7
      DO 20 I = 1, 6
         CONST(I) = 0.0D0
   20 CONTINUE
      IFAIL = 0
*
      CALL D02NVF(NEQMAX,NY2DIM,MAXORD,'Functional-iteration',PETZLD,
     +            CONST,TCRIT,HMIN,HMAX,H0,MAXSTP,MXHNIL,'Average-L2',
     +            RWORK,IFAIL)
*     Linear algebra setup required (in case functional iteration
*     encounters any difficulty).
      CALL D02NSF(NEQ,NEQMAX,'Numerical',NWKJAC,RWORK,IFAIL)
*
      XOUT = 0.02D0
      IOUT = 1
*
      WRITE (NOUT,*)
      WRITE (NOUT,*) '    X          Y(1)           Y(2)           Y(3)'
      WRITE (NOUT,99999) T, (Y(I),I=1,NEQ)
*
*     Soft fail and error messages only
      ITRACE = 0
   40 IFAIL = 1
*
      CALL D02NGF(NEQ,NEQMAX,T,TOUT,Y,YDOT,RWORK,RTOL,ATOL,ITOL,INFORM,
     +            RESID,YSAVE,NY2DIM,D02NGZ,WKJAC,NWKJAC,D02NBY,LDERIV,
     +            ITASK,ITRACE,IFAIL)
*
      IF (IFAIL.EQ.0) THEN
*
         CALL D02NYF(NEQ,NEQMAX,HU,H,TCUR,TOLSF,RWORK,NST,NRE,NJE,NQU,
     +               NQ,NITER,IMXER,ALGEQU,INFORM,IFAIL)
*
   60    CONTINUE
         IF (TCUR-HU.LT.XOUT .AND. XOUT.LE.TCUR) THEN
            IFAIL = 0
*           C0 interpolation
            CALL D02XJF(XOUT,SOL,NEQ,YSAVE,NEQMAX,NY2DIM,NEQ,TCUR,NQU,
     +                  HU,H,IFAIL)
*
            WRITE (NOUT,99999) XOUT, (SOL(I),I=1,NEQ)
            IOUT = IOUT + 1
            XOUT = DBLE(IOUT)*0.02D0
            IF (IOUT.LT.6) THEN
               GO TO 60
            ELSE
               WRITE (NOUT,*)
               WRITE (NOUT,99997) ' HUSED = ', HU, '  HNEXT = ', H,
     +           '  TCUR = ', TCUR
               WRITE (NOUT,99996) ' NST = ', NST, '    NRE = ', NRE,
     +           '    NJE = ', NJE
               WRITE (NOUT,99996) ' NQU = ', NQU, '    NQ  = ', NQ,
     +           '  NITER = ', NITER
               WRITE (NOUT,99995) ' Max err comp = ', IMXER
            END IF
         ELSE
            GO TO 40
         END IF
      ELSE
         WRITE (NOUT,*)
         WRITE (NOUT,99998) 'Exit D02NGF with IFAIL = ', IFAIL,
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
      SUBROUTINE RESID(NEQ,T,Y,YDOT,R,IRES)
*     .. Scalar Arguments ..
      DOUBLE PRECISION T
      INTEGER          IRES, NEQ
*     .. Array Arguments ..
      DOUBLE PRECISION R(NEQ), Y(NEQ), YDOT(NEQ)
*     .. Executable Statements ..
      R(1) = -YDOT(1)
      R(2) = -YDOT(2)
      R(3) = -YDOT(3)
*
      IF (IRES.EQ.1) THEN
         R(1) = -0.04D0*Y(1) + 1.0D4*Y(2)*Y(3) + R(1)
         R(2) = 0.04D0*Y(1) - 1.0D4*Y(2)*Y(3) - 3.0D7*Y(2)*Y(2) + R(2)
         R(3) = 3.0D7*Y(2)*Y(2) + R(3)
      END IF
      RETURN
      END
