*     D03PCF Example Program Text
*     Mark 15 Release. NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          NPDE, NPTS, INTPTS, ITYPE, NEQN, NIW, NWK, NW
      PARAMETER        (NPDE=2,NPTS=20,INTPTS=6,ITYPE=1,NEQN=NPDE*NPTS,
     +                 NIW=NEQN+24,NWK=(10+6*NPDE)*NEQN,
     +                 NW=NWK+(21+3*NPDE)*NPDE+7*NPTS+54)
*     .. Scalars in Common ..
      DOUBLE PRECISION ALPHA
*     .. Local Scalars ..
      DOUBLE PRECISION ACC, HX, PI, PIBY2, TOUT, TS
      INTEGER          I, IFAIL, IND, IT, ITASK, ITRACE, M
*     .. Local Arrays ..
      DOUBLE PRECISION U(NPDE,NPTS), UOUT(NPDE,INTPTS,ITYPE), W(NW),
     +                 X(NPTS), XOUT(INTPTS)
      INTEGER          IW(NIW)
*     .. External Functions ..
      DOUBLE PRECISION X01AAF
      EXTERNAL         X01AAF
*     .. External Subroutines ..
      EXTERNAL         BNDARY, D03PCF, D03PZF, PDEDEF, UINIT
*     .. Intrinsic Functions ..
      INTRINSIC        SIN
*     .. Common blocks ..
      COMMON           /VBLE/ALPHA
*     .. Data statements ..
      DATA             XOUT(1)/0.0D+0/, XOUT(2)/0.40D+0/,
     +                 XOUT(3)/0.6D+0/, XOUT(4)/0.8D+0/,
     +                 XOUT(5)/0.9D+0/, XOUT(6)/1.0D+0/
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D03PCF Example Program Results'
      ACC = 1.0D-4
      M = 1
      ITRACE = 0
      ALPHA = 1.0D0
      IND = 0
      ITASK = 1
*
*     Set spatial mesh points
*
      PIBY2 = 0.5D0*X01AAF(PI)
      HX = PIBY2/(NPTS-1)
      X(1) = 0.0D0
      X(NPTS) = 1.0D0
      DO 20 I = 2, NPTS - 1
         X(I) = SIN(HX*(I-1))
   20 CONTINUE
*
*     Set initial conditions
*
      TS = 0.0D0
      TOUT = 0.1D-4
      WRITE (NOUT,99999) ACC, ALPHA
      WRITE (NOUT,99998) (XOUT(I),I=1,6)
*
*     Set the initial values
*
      CALL UINIT(U,X,NPTS)
      DO 40 IT = 1, 5
         IFAIL = -1
         TOUT = 10.0D0*TOUT
*
         CALL D03PCF(NPDE,M,TS,TOUT,PDEDEF,BNDARY,U,NPTS,X,ACC,W,NW,IW,
     +               NIW,ITASK,ITRACE,IND,IFAIL)
*
*        Interpolate at required spatial points
*
         CALL D03PZF(NPDE,M,U,NPTS,X,XOUT,INTPTS,ITYPE,UOUT,IFAIL)
         WRITE (NOUT,99996) TOUT, (UOUT(1,I,1),I=1,INTPTS)
         WRITE (NOUT,99995) (UOUT(2,I,1),I=1,INTPTS)
   40 CONTINUE
*
*     Print integration statistics
*
      WRITE (NOUT,99997) IW(1), IW(2), IW(3), IW(5)
      STOP
*
99999 FORMAT (//' Accuracy requirement  = ',D12.5,/' Parameter ALPHA =',
     +       '       ',D12.3,/)
99998 FORMAT ('   T  /  X  ',6F8.4,/)
99997 FORMAT (' Number of integration steps in time                  ',
     +       I4,/' Number of residual evaluations of resulting ODE sys',
     +       'tem',I4,/' Number of Jacobian evaluations               ',
     +       '         ',I4,/' Number of iterations of nonlinear solve',
     +       'r              ',I4,/)
99996 FORMAT (1X,F6.4,' U(1)',6F8.4)
99995 FORMAT (8X,'U(2)',6F8.4,/)
      END
*
      SUBROUTINE UINIT(U,X,NPTS)
*     Routine for PDE initial conditon
*     .. Scalar Arguments ..
      INTEGER          NPTS
*     .. Array Arguments ..
      DOUBLE PRECISION U(2,NPTS), X(NPTS)
*     .. Scalars in Common ..
      DOUBLE PRECISION ALPHA
*     .. Local Scalars ..
      INTEGER          I
*     .. Common blocks ..
      COMMON           /VBLE/ALPHA
*     .. Executable Statements ..
      DO 20 I = 1, NPTS
         U(1,I) = 2.0D0*ALPHA*X(I)
         U(2,I) = 1.0D0
   20 CONTINUE
      RETURN
      END
*
      SUBROUTINE PDEDEF(NPDE,T,X,U,DUDX,P,Q,R,IRES)
*     .. Scalar Arguments ..
      DOUBLE PRECISION  T, X
      INTEGER           IRES, NPDE
*     .. Array Arguments ..
      DOUBLE PRECISION  DUDX(NPDE), P(NPDE,NPDE), Q(NPDE), R(NPDE),
     +                  U(NPDE)
*     .. Scalars in Common ..
      DOUBLE PRECISION  ALPHA
*     .. Common blocks ..
      COMMON            /VBLE/ALPHA
*     .. Executable Statements ..
      Q(1) = 4.0D0*ALPHA*(U(2)+X*DUDX(2))
      Q(2) = 0.0D+0
      R(1) = X*DUDX(1)
      R(2) = DUDX(2) - U(1)*U(2)
      P(1,1) = 0.0D+0
      P(1,2) = 0.0D0
      P(2,1) = 0.0D+0
      P(2,2) = 1.0D0 - X*X
      RETURN
      END
*
      SUBROUTINE BNDARY(NPDE,T,U,UX,IBND,BETA,GAMMA,IRES)
*     .. Scalar Arguments ..
      DOUBLE PRECISION  T
      INTEGER           IBND, IRES, NPDE
*     .. Array Arguments ..
      DOUBLE PRECISION  BETA(NPDE), GAMMA(NPDE), U(NPDE), UX(NPDE)
*     .. Executable Statements ..
      IF (IBND.EQ.0) THEN
         BETA(1) = 0.0D+0
         BETA(2) = 1.0D+0
         GAMMA(1) = U(1)
         GAMMA(2) = -U(1)*U(2)
      ELSE
         BETA(1) = 1.0D0
         BETA(2) = 0.0D+0
         GAMMA(1) = -U(1)
         GAMMA(2) = U(2)
      END IF
      RETURN
      END
