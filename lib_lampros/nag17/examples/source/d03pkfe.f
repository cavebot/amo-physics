*     D03PKF Example Program Text
*     Mark 16 Release. NAG Copyright 1993.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          NPDE, NPTS, NCODE, NXI, NLEFT, NEQN, NIW, NWKRES,
     +                 LENODE, NW
      PARAMETER        (NPDE=2,NPTS=21,NCODE=1,NXI=1,NLEFT=1,
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
      DOUBLE PRECISION ALGOPT(30), ATOL(1), EXY(NEQN), RTOL(1), U(NEQN),
     +                 W(NW), X(NPTS), XI(1)
      INTEGER          IW(NIW)
*     .. External Subroutines ..
      EXTERNAL         BNDARY, D03PKF, EXACT, ODEDEF, PDEDEF, UVINIT
*     .. Common blocks ..
      COMMON           /TAXIS/TS
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D03PKF Example Program Results'
      ITRACE = 0
      ITOL = 1
      ATOL(1) = 0.1D-3
      RTOL(1) = ATOL(1)
      WRITE (NOUT,99997) ATOL, NPTS
*
*     Set spatial-mesh points
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
      ALGOPT(1) = 1.0D0
      ALGOPT(13) = 0.5D-2
*
*     Loop over output value of t
*
      TS = 1.0D-4
      TOUT = 0.0D0
      WRITE (NOUT,99999) X(1), X(5), X(9), X(13), X(21)
*
      CALL UVINIT(NPDE,NPTS,X,U,NCODE,NEQN)
*
      DO 60 IT = 1, 5
         TOUT = 0.1D0*(2**IT)
         IFAIL = -1
*
         CALL D03PKF(NPDE,TS,TOUT,PDEDEF,BNDARY,U,NPTS,X,NLEFT,NCODE,
     +               ODEDEF,NXI,XI,NEQN,RTOL,ATOL,ITOL,NORM,LAOPT,
     +               ALGOPT,W,NW,IW,NIW,ITASK,ITRACE,IND,IFAIL)
*
*        Check against the exact solution
*
         CALL EXACT(TOUT,NEQN,NPTS,X,EXY)
*
         WRITE (NOUT,99998) TS
         WRITE (NOUT,99995) U(1), U(9), U(17), U(25), U(41), U(43)
         WRITE (NOUT,99994) EXY(1), EXY(9), EXY(17), EXY(25),
     +     EXY(41), TS
   60 CONTINUE
      WRITE (NOUT,99996) IW(1), IW(2), IW(3), IW(5)
      STOP
*
99999 FORMAT ('  X        ',5F9.3,/)
99998 FORMAT (' T = ',F6.3)
99997 FORMAT (//'  Accuracy requirement =',D10.3,' Number of points = ',
     +       I3,/)
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
      INTEGER           I, K
*     .. Intrinsic Functions ..
      INTRINSIC         EXP
*     .. Common blocks ..
      COMMON            /TAXIS/TS
*     .. Executable Statements ..
      K = 1
      DO 20 I = 1, NPTS
         U(K) = EXP(TS*(1.0D0-X(I))) - 1.0D0
         U(K+1) = -TS*EXP(TS*(1.0D0-X(I)))
         K = K + 2
   20 CONTINUE
      U(NEQN) = TS
      RETURN
      END
*
      SUBROUTINE ODEDEF(NPDE,T,NCODE,V,VDOT,NXI,XI,UCP,UCPX,UCPT,F,IRES)
*     .. Scalar Arguments ..
      DOUBLE PRECISION  T
      INTEGER           IRES, NCODE, NPDE, NXI
*     .. Array Arguments ..
      DOUBLE PRECISION  F(*), UCP(NPDE,*), UCPT(NPDE,*), UCPX(NPDE,*),
     +                  V(*), VDOT(*), XI(*)
*     .. Executable Statements ..
      IF (IRES.EQ.-1) THEN
         F(1) = VDOT(1)
      ELSE
         F(1) = VDOT(1) - V(1)*UCP(1,1) - UCP(2,1) - 1.0D0 - T
      END IF
      RETURN
      END
*
      SUBROUTINE PDEDEF(NPDE,T,X,U,UDOT,UX,NCODE,V,VDOT,RES,IRES)
*     .. Scalar Arguments ..
      DOUBLE PRECISION  T, X
      INTEGER           IRES, NCODE, NPDE
*     .. Array Arguments ..
      DOUBLE PRECISION  RES(NPDE), U(NPDE), UDOT(NPDE), UX(NPDE), V(*),
     +                  VDOT(*)
*     .. Executable Statements ..
      IF (IRES.EQ.-1) THEN
         RES(1) = V(1)*V(1)*UDOT(1) - X*U(2)*V(1)*VDOT(1)
         RES(2) = 0.0D0
      ELSE
         RES(1) = V(1)*V(1)*UDOT(1) - X*U(2)*V(1)*VDOT(1) - UX(2)
         RES(2) = U(2) - UX(1)
      END IF
      RETURN
      END
*
      SUBROUTINE BNDARY(NPDE,T,IBND,NOBC,U,UDOT,NCODE,V,VDOT,RES,IRES)
*     .. Scalar Arguments ..
      DOUBLE PRECISION  T
      INTEGER           IBND, IRES, NCODE, NOBC, NPDE
*     .. Array Arguments ..
      DOUBLE PRECISION  RES(NOBC), U(NPDE), UDOT(NPDE), V(*), VDOT(*)
*     .. Intrinsic Functions ..
      INTRINSIC         EXP
*     .. Executable Statements ..
      IF (IBND.EQ.0) THEN
         IF (IRES.EQ.-1) THEN
            RES(1) = 0.0D0
         ELSE
            RES(1) = U(2) + V(1)*EXP(T)
         END IF
      ELSE
         IF (IRES.EQ.-1) THEN
            RES(1) = V(1)*VDOT(1)
         ELSE
            RES(1) = U(2) + V(1)*VDOT(1)
         END IF
      END IF
      RETURN
      END
*
      SUBROUTINE EXACT(TIME,NEQN,NPTS,X,U)
*     Exact solution (for comparison purposes)
*     .. Scalar Arguments ..
      DOUBLE PRECISION TIME
      INTEGER          NEQN, NPTS
*     .. Array Arguments ..
      DOUBLE PRECISION U(NEQN), X(NPTS)
*     .. Local Scalars ..
      INTEGER          I, K
*     .. Intrinsic Functions ..
      INTRINSIC        EXP
*     .. Executable Statements ..
      K = 1
      DO 20 I = 1, NPTS
         U(K) = EXP(TIME*(1.0D0-X(I))) - 1.0D0
         K = K + 2
   20 CONTINUE
      RETURN
      END
