*     D03PEF Example Program Text
*     Mark 16 Release. NAG Copyright 1993.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          NPDE, NPTS, NLEFT, NEQN, NIW, NWKRES, NW
      PARAMETER        (NPDE=2,NPTS=41,NLEFT=1,NEQN=NPDE*NPTS,
     +                 NIW=NEQN+24,NWKRES=NPDE*(NPTS+21+3*NPDE)
     +                 +7*NPTS+4,NW=11*NEQN+(4*NPDE+NLEFT+2)
     +                 *NEQN+50+NWKRES)
*     .. Local Scalars ..
      DOUBLE PRECISION ACC, TOUT, TS
      INTEGER          I, IFAIL, IND, IT, ITASK, ITRACE
*     .. Local Arrays ..
      DOUBLE PRECISION EU(NPDE,NPTS), U(NPDE,NPTS), W(NW), X(NPTS)
      INTEGER          IW(NIW)
*     .. External Subroutines ..
      EXTERNAL         BNDARY, D03PEF, EXACT, PDEDEF, UINIT
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D03PEF Example Program Results'
      ITRACE = 0
      ACC = 0.1D-5
      WRITE (NOUT,99997) ACC, NPTS
*
*     Set spatial-mesh points
*
      DO 20 I = 1, NPTS
         X(I) = (I-1.0D0)/(NPTS-1.0D0)
   20 CONTINUE
      WRITE (NOUT,99999) X(5), X(13), X(21), X(29), X(37)
*
      IND = 0
      ITASK = 1
*
      CALL UINIT(NPDE,NPTS,X,U)
*
*     Loop over output value of t
      TS = 0.0D0
      TOUT = 0.0D0
      DO 40 IT = 1, 5
         TOUT = 0.2D0*IT
         IFAIL = -1
*
         CALL D03PEF(NPDE,TS,TOUT,PDEDEF,BNDARY,U,NPTS,X,NLEFT,ACC,W,NW,
     +               IW,NIW,ITASK,ITRACE,IND,IFAIL)
*
*        Check against the exact solution
*
         CALL EXACT(TOUT,NPDE,NPTS,X,EU)
*
         WRITE (NOUT,99998) TS
         WRITE (NOUT,99995) U(1,5), U(1,13), U(1,21), U(1,29),
     +     U(1,37)
         WRITE (NOUT,99994) EU(1,5), EU(1,13), EU(1,21), EU(1,29),
     +     EU(1,37)
         WRITE (NOUT,99993) U(2,5), U(2,13), U(2,21), U(2,29),
     +     U(2,37)
         WRITE (NOUT,99992) EU(2,5), EU(2,13), EU(2,21), EU(2,29),
     +     EU(2,37)
   40 CONTINUE
      WRITE (NOUT,99996) IW(1), IW(2), IW(3), IW(5)
      STOP
*
99999 FORMAT (' X        ',5F10.4,/)
99998 FORMAT (' T = ',F5.2)
99997 FORMAT (//'  Accuracy requirement =',D10.3,' Number of points = ',
     +       I3,/)
99996 FORMAT (' Number of integration steps in time = ',I6,/' Number o',
     +       'f function evaluations = ',I6,/' Number of Jacobian eval',
     +       'uations =',I6,/' Number of iterations = ',I6,/)
99995 FORMAT (' Approx U1',5F10.4)
99994 FORMAT (' Exact  U1',5F10.4)
99993 FORMAT (' Approx U2',5F10.4)
99992 FORMAT (' Exact  U2',5F10.4,/)
      END
*
      SUBROUTINE UINIT(NPDE,NPTS,X,U)
*     Routine for PDE initial values
*     .. Scalar Arguments ..
      INTEGER          NPDE, NPTS
*     .. Array Arguments ..
      DOUBLE PRECISION U(NPDE,NPTS), X(NPTS)
*     .. Local Scalars ..
      INTEGER          I
*     .. Intrinsic Functions ..
      INTRINSIC        EXP, SIN
*     .. Executable Statements ..
      DO 20 I = 1, NPTS
         U(1,I) = EXP(X(I))
         U(2,I) = SIN(X(I))
   20 CONTINUE
      RETURN
      END
*
      SUBROUTINE PDEDEF(NPDE,T,X,U,UDOT,DUDX,RES,IRES)
*     .. Scalar Arguments ..
      DOUBLE PRECISION  T, X
      INTEGER           IRES, NPDE
*     .. Array Arguments ..
      DOUBLE PRECISION  DUDX(NPDE), RES(NPDE), U(NPDE), UDOT(NPDE)
*     .. Executable Statements ..
      IF (IRES.EQ.-1) THEN
         RES(1) = UDOT(1)
         RES(2) = UDOT(2)
      ELSE
         RES(1) = UDOT(1) + DUDX(1) + DUDX(2)
         RES(2) = UDOT(2) + 4.0D0*DUDX(1) + DUDX(2)
      END IF
      RETURN
      END
*
      SUBROUTINE BNDARY(NPDE,T,IBND,NOBC,U,UDOT,RES,IRES)
*     .. Scalar Arguments ..
      DOUBLE PRECISION  T
      INTEGER           IBND, IRES, NOBC, NPDE
*     .. Array Arguments ..
      DOUBLE PRECISION  RES(NOBC), U(NPDE), UDOT(NPDE)
*     .. Intrinsic Functions ..
      INTRINSIC         EXP, SIN
*     .. Executable Statements ..
      IF (IBND.EQ.0) THEN
         IF (IRES.EQ.-1) THEN
            RES(1) = 0.0D0
         ELSE
            RES(1) = U(1) - 0.5D0*(EXP(T)+EXP(-3.0D0*T)) -
     +               0.25D0*(SIN(-3.0D0*T)-SIN(T))
         END IF
      ELSE
         IF (IRES.EQ.-1) THEN
            RES(1) = 0.0D0
         ELSE
            RES(1) = U(2) - EXP(1.0D0-3.0D0*T) + EXP(1.0D0+T) -
     +               0.5D0*(SIN(1.0D0-3.0D0*T)+SIN(1.0D0+T))
         END IF
      END IF
      RETURN
      END
*
      SUBROUTINE EXACT(T,NPDE,NPTS,X,U)
*     Exact solution (for comparison purposes)
*     .. Scalar Arguments ..
      DOUBLE PRECISION T
      INTEGER          NPDE, NPTS
*     .. Array Arguments ..
      DOUBLE PRECISION U(NPDE,NPTS), X(NPTS)
*     .. Local Scalars ..
      INTEGER          I
*     .. Intrinsic Functions ..
      INTRINSIC        EXP, SIN
*     .. Executable Statements ..
      DO 20 I = 1, NPTS
         U(1,I) = 0.5D0*(EXP(X(I)+T)+EXP(X(I)-3.0D0*T)) +
     +            0.25D0*(SIN(X(I)-3.0D0*T)-SIN(X(I)+T))
         U(2,I) = EXP(X(I)-3.0D0*T) - EXP(X(I)+T) + 0.5D0*(SIN(X(I)
     +            -3.0D0*T)+SIN(X(I)+T))
   20 CONTINUE
      RETURN
      END
