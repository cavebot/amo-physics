*     D03PHF Example Program Text
*     Mark 16 Revised. NAG Copyright 1993.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          NPDE, NPTS, NCODE, M, NXI, NEQN, NIW, NWKRES,
     +                 LENODE, NW
      PARAMETER        (NPDE=1,NPTS=21,NCODE=1,M=0,NXI=1,
     +                 NEQN=NPDE*NPTS+NCODE,NIW=24,
     +                 NWKRES=NPDE*(NPTS+6*NXI+3*NPDE+15)
     +                 +NCODE+NXI+7*NPTS+2,LENODE=11*NEQN+50,
     +                 NW=NEQN*NEQN+NEQN+NWKRES+LENODE)
*     .. Scalars in Common ..
      DOUBLE PRECISION TS
*     .. Local Scalars ..
      DOUBLE PRECISION TOUT
      INTEGER          I, IFAIL, IND, IT, ITASK, ITOL, ITRACE
      LOGICAL          THETA
      CHARACTER        LAOPT, NORM
*     .. Local Arrays ..
      DOUBLE PRECISION ALGOPT(30), ATOL(1), EXY(NPTS), RTOL(1), U(NEQN),
     +                 W(NW), X(NPTS), XI(1)
      INTEGER          IW(NIW)
*     .. External Subroutines ..
      EXTERNAL         BNDARY, D03PHF, EXACT, ODEDEF, PDEDEF, UVINIT
*     .. Common blocks ..
      COMMON           /TAXIS/TS
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D03PHF Example Program Results'
      ITRACE = 0
      ITOL = 1
      ATOL(1) = 1.0D-4
      RTOL(1) = ATOL(1)
      WRITE (NOUT,99997) ATOL, NPTS
*
*     Set break-points
*
      DO 20 I = 1, NPTS
         X(I) = (I-1.0D0)/(NPTS-1.0D0)
   20 CONTINUE
*
      XI(1) = 1.0D0
      NORM = 'A'
      LAOPT = 'F'
      IND = 0
      ITASK = 1
*
*     Set THETA to .TRUE. if the Theta integrator is required
*
      THETA = .FALSE.
      DO 40 I = 1, 30
         ALGOPT(I) = 0.0D0
   40 CONTINUE
      IF (THETA) THEN
         ALGOPT(1) = 2.0D0
      ELSE
         ALGOPT(1) = 0.0D0
      END IF
*
*     Loop over output value of t
*
      TS = 1.0D-4
      TOUT = 0.0D0
      WRITE (NOUT,99999) X(1), X(5), X(9), X(13), X(21)
      CALL UVINIT(NPDE,NPTS,X,U,NCODE,NEQN)
      DO 60 IT = 1, 5
         TOUT = 0.1D0*(2**IT)
         IFAIL = -1
*
         CALL D03PHF(NPDE,M,TS,TOUT,PDEDEF,BNDARY,U,NPTS,X,NCODE,ODEDEF,
     +               NXI,XI,NEQN,RTOL,ATOL,ITOL,NORM,LAOPT,ALGOPT,W,NW,
     +               IW,NIW,ITASK,ITRACE,IND,IFAIL)
*
*        Check against the exact solution
*
         CALL EXACT(TOUT,NPTS,X,EXY)
         WRITE (NOUT,99998) TS
         WRITE (NOUT,99995) U(1), U(5), U(9), U(13), U(21), U(22)
         WRITE (NOUT,99994) EXY(1), EXY(5), EXY(9), EXY(13),
     +     EXY(21), TS
   60 CONTINUE
      WRITE (NOUT,99996) IW(1), IW(2), IW(3), IW(5)
      STOP
*
99999 FORMAT ('  X        ',5F9.3,/)
99998 FORMAT (' T = ',F6.3)
99997 FORMAT (//'  Simple coupled PDE using BDF ',/'  Accuracy require',
     +       'ment =',D10.3,' Number of points = ',I4,/)
99996 FORMAT (' Number of integration steps in time = ',I6,/' Number o',
     +       'f function evaluations = ',I6,/' Number of Jacobian eval',
     +       'uations =',I6,/' Number of iterations = ',I6,/)
99995 FORMAT (1X,'App.  sol.  ',F7.3,4F9.3,'  ODE sol. =',F8.3)
99994 FORMAT (1X,'Exact sol.  ',F7.3,4F9.3,'  ODE sol. =',F8.3,/)
      END
*
      SUBROUTINE UVINIT(NPDE,NPTS,X,U,NCODE,NEQN)
*     Routine for PDE initial values
*     .. Scalar Arguments ..
      INTEGER           NCODE, NEQN, NPDE, NPTS
*     .. Array Arguments ..
      DOUBLE PRECISION  U(NEQN), X(NPTS)
*     .. Scalars in Common ..
      DOUBLE PRECISION  TS
*     .. Local Scalars ..
      INTEGER           I
*     .. Intrinsic Functions ..
      INTRINSIC         EXP
*     .. Common blocks ..
      COMMON            /TAXIS/TS
*     .. Executable Statements ..
      DO 20 I = 1, NPTS
         U(I) = EXP(TS*(1.0D0-X(I))) - 1.0D0
   20 CONTINUE
      U(NEQN) = TS
      RETURN
      END
*
      SUBROUTINE ODEDEF(NPDE,T,NCODE,V,VDOT,NXI,XI,UCP,UCPX,RCP,UCPT,
     +                  UCPTX,F,IRES)
*     .. Scalar Arguments ..
      DOUBLE PRECISION  T
      INTEGER           IRES, NCODE, NPDE, NXI
*     .. Array Arguments ..
      DOUBLE PRECISION  F(*), RCP(NPDE,*), UCP(NPDE,*), UCPT(NPDE,*),
     +                  UCPTX(NPDE,*), UCPX(NPDE,*), V(*), VDOT(*),
     +                  XI(*)
*     .. Executable Statements ..
      IF (IRES.EQ.1) THEN
         F(1) = VDOT(1) - V(1)*UCP(1,1) - UCPX(1,1) - 1.0D0 - T
      ELSE IF (IRES.EQ.-1) THEN
         F(1) = VDOT(1)
      END IF
      RETURN
      END
*
      SUBROUTINE PDEDEF(NPDE,T,X,U,UX,NCODE,V,VDOT,P,Q,R,IRES)
*     .. Scalar Arguments ..
      DOUBLE PRECISION  T, X
      INTEGER           IRES, NCODE, NPDE
*     .. Array Arguments ..
      DOUBLE PRECISION  P(NPDE,NPDE), Q(NPDE), R(NPDE), U(NPDE),
     +                  UX(NPDE), V(*), VDOT(*)
*     .. Executable Statements ..
      P(1,1) = V(1)*V(1)
      R(1) = UX(1)
      Q(1) = -X*UX(1)*V(1)*VDOT(1)
      RETURN
      END
*
      SUBROUTINE BNDARY(NPDE,T,U,UX,NCODE,V,VDOT,IBND,BETA,GAMMA,IRES)
*     .. Scalar Arguments ..
      DOUBLE PRECISION  T
      INTEGER           IBND, IRES, NCODE, NPDE
*     .. Array Arguments ..
      DOUBLE PRECISION  BETA(NPDE), GAMMA(NPDE), U(NPDE), UX(NPDE),
     +                  V(*), VDOT(*)
*     .. Intrinsic Functions ..
      INTRINSIC         EXP
*     .. Executable Statements ..
      BETA(1) = 1.0D0
      IF (IBND.EQ.0) THEN
         GAMMA(1) = -V(1)*EXP(T)
      ELSE
         GAMMA(1) = -V(1)*VDOT(1)
      END IF
      RETURN
      END
*
      SUBROUTINE EXACT(TIME,NPTS,X,U)
*     Exact solution (for comparison purpose)
*     .. Scalar Arguments ..
      DOUBLE PRECISION TIME
      INTEGER          NPTS
*     .. Array Arguments ..
      DOUBLE PRECISION U(NPTS), X(NPTS)
*     .. Local Scalars ..
      INTEGER          I
*     .. Intrinsic Functions ..
      INTRINSIC        EXP
*     .. Executable Statements ..
      DO 20 I = 1, NPTS
         U(I) = EXP(TIME*(1.0D0-X(I))) - 1.0D0
   20 CONTINUE
      RETURN
      END
