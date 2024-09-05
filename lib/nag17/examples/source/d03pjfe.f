*     D03PJF Example Program Text
*     Mark 16 Revised. NAG Copyright 1993.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          NBKPTS, NEL, NPDE, NPOLY, NPTS, NCODE, M, NXI,
     +                 NEQN, NIW, NPL1, NWKRES, LENODE, NW
      PARAMETER        (NBKPTS=11,NEL=NBKPTS-1,NPDE=1,NPOLY=2,
     +                 NPTS=NEL*NPOLY+1,NCODE=1,M=0,NXI=1,
     +                 NEQN=NPDE*NPTS+NCODE,NIW=24,NPL1=NPOLY+1,
     +                 NWKRES=3*NPL1*NPL1+NPL1*
     +                 (NPDE*NPDE+6*NPDE+NBKPTS+1)+8*NPDE+NXI*(5*NPDE+1)
     +                 +NCODE+3,LENODE=11*NEQN+50,
     +                 NW=NEQN*NEQN+NEQN+NWKRES+LENODE)
*     .. Scalars in Common ..
      DOUBLE PRECISION TS
*     .. Local Scalars ..
      DOUBLE PRECISION TOUT
      INTEGER          I, IFAIL, IND, IT, ITASK, ITOL, ITRACE
      LOGICAL          THETA
      CHARACTER        LAOPT, NORM
*     .. Local Arrays ..
      DOUBLE PRECISION ALGOPT(30), ATOL(1), EXY(NBKPTS), RTOL(1),
     +                 U(NEQN), W(NW), X(NPTS), XBKPTS(NBKPTS), XI(1)
      INTEGER          IW(NIW)
*     .. External Subroutines ..
      EXTERNAL         BNDARY, D03PJF, EXACT, ODEDEF, PDEDEF, UVINIT
*     .. Common blocks ..
      COMMON           /TAXIS/TS
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D03PJF Example Program Results'
      ITRACE = 0
      ITOL = 1
      ATOL(1) = 1.0D-4
      RTOL(1) = ATOL(1)
      WRITE (NOUT,99999) NPOLY, NEL
      WRITE (NOUT,99996) ATOL, NPTS
*
*     Set break-points
*
      DO 20 I = 1, NBKPTS
         XBKPTS(I) = (I-1.0D0)/(NBKPTS-1.0D0)
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
      WRITE (NOUT,99998) XBKPTS(1), XBKPTS(3), XBKPTS(5), XBKPTS(7),
     +  XBKPTS(11)
      DO 60 IT = 1, 5
         TOUT = 0.1D0*(2**IT)
         IFAIL = -1
*
         CALL D03PJF(NPDE,M,TS,TOUT,PDEDEF,BNDARY,U,NBKPTS,XBKPTS,NPOLY,
     +               NPTS,X,NCODE,ODEDEF,NXI,XI,NEQN,UVINIT,RTOL,ATOL,
     +               ITOL,NORM,LAOPT,ALGOPT,W,NW,IW,NIW,ITASK,ITRACE,
     +               IND,IFAIL)
*
*        Check against the exact solution
*
         CALL EXACT(TOUT,NBKPTS,XBKPTS,EXY)
         WRITE (NOUT,99997) TS
         WRITE (NOUT,99994) U(1), U(5), U(9), U(13), U(21), U(22)
         WRITE (NOUT,99993) EXY(1), EXY(3), EXY(5), EXY(7), EXY(11),
     +     TS
   60 CONTINUE
      WRITE (NOUT,99995) IW(1), IW(2), IW(3), IW(5)
      STOP
*
99999 FORMAT (' Degree of Polynomial =',I4,'   No. of elements =',I4,/)
99998 FORMAT ('  X        ',5F9.3,/)
99997 FORMAT (' T = ',F6.3)
99996 FORMAT (//'  Simple coupled PDE using BDF ',/'  Accuracy require',
     +       'ment =',D10.3,' Number of points = ',I4,/)
99995 FORMAT (' Number of integration steps in time = ',I6,/' Number o',
     +       'f function evaluations = ',I6,/' Number of Jacobian eval',
     +       'uations =',I6,/' Number of iterations = ',I6,/)
99994 FORMAT (1X,'App.  sol.  ',F7.3,4F9.3,'  ODE sol. =',F8.3)
99993 FORMAT (1X,'Exact sol.  ',F7.3,4F9.3,'  ODE sol. =',F8.3,/)
      END
*
      SUBROUTINE UVINIT(NPDE,NPTS,X,U,NCODE,V)
*     Routine for PDE initial values (start time is 0.1D-6)
*     .. Scalar Arguments ..
      INTEGER           NCODE, NPDE, NPTS
*     .. Array Arguments ..
      DOUBLE PRECISION  U(NPDE,NPTS), V(*), X(NPTS)
*     .. Scalars in Common ..
      DOUBLE PRECISION  TS
*     .. Local Scalars ..
      INTEGER           I
*     .. Intrinsic Functions ..
      INTRINSIC         EXP
*     .. Common blocks ..
      COMMON            /TAXIS/TS
*     .. Executable Statements ..
      V(1) = TS
      DO 20 I = 1, NPTS
         U(1,I) = EXP(TS*(1.0D0-X(I))) - 1.0D0
   20 CONTINUE
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
      SUBROUTINE PDEDEF(NPDE,T,X,NPTL,U,DUDX,NCODE,V,VDOT,P,Q,R,IRES)
*     .. Scalar Arguments ..
      DOUBLE PRECISION  T
      INTEGER           IRES, NCODE, NPDE, NPTL
*     .. Array Arguments ..
      DOUBLE PRECISION  DUDX(NPDE,NPTL), P(NPDE,NPDE,NPTL),
     +                  Q(NPDE,NPTL), R(NPDE,NPTL), U(NPDE,NPTL), V(*),
     +                  VDOT(*), X(NPTL)
*     .. Local Scalars ..
      INTEGER           I
*     .. Executable Statements ..
      DO 20 I = 1, NPTL
         P(1,1,I) = V(1)*V(1)
         R(1,I) = DUDX(1,I)
         Q(1,I) = -X(I)*DUDX(1,I)*V(1)*VDOT(1)
   20 CONTINUE
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
*     Exact solution (for comparison purposes)
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
