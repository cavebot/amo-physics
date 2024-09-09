*     D03PRF Example Program Text
*     Mark 16 Release. NAG Copyright 1993.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          NPDE, NPTS, NV, NXI, NXFIX, NLEFT, NEQN, NIW,
     +                 NWKRES, LENODE, NW, INTPTS, ITYPE
      PARAMETER        (NPDE=2,NPTS=61,NV=0,NXI=0,NXFIX=0,NLEFT=1,
     +                 NEQN=NPDE*NPTS+NV,NIW=25+NXFIX,
     +                 NWKRES=NPDE*(NPTS+21+3*NPDE)+7*NPTS+NXFIX+3,
     +                 LENODE=11*NEQN+50,NW=NEQN*NEQN+NEQN+NWKRES+
     +                 LENODE,INTPTS=5,ITYPE=1)
*     .. Scalars in Common ..
      DOUBLE PRECISION P
*     .. Local Scalars ..
      DOUBLE PRECISION CONST, DXMESH, TOUT, TRMESH, TS, XRATIO, XX
      INTEGER          I, IFAIL, IND, IPMINF, IT, ITASK, ITOL, ITRACE,
     +                 NRMESH
      LOGICAL          REMESH, THETA
      CHARACTER        LAOPT, NORM
*     .. Local Arrays ..
      DOUBLE PRECISION ALGOPT(30), ATOL(1), RTOL(1), U(NPDE,NPTS),
     +                 UE(NPDE,NPTS), UOUT(NPDE,INTPTS,ITYPE), W(NW),
     +                 X(NPTS), XFIX(1), XI(1), XOUT(INTPTS)
      INTEGER          IW(NIW)
*     .. External Functions ..
      DOUBLE PRECISION X01AAF
      EXTERNAL         X01AAF
*     .. External Subroutines ..
      EXTERNAL         BNDARY, D03PEK, D03PRF, D03PZF, EXACT, MONITF,
     +                 PDEDEF, UVINIT
*     .. Common blocks ..
      COMMON           /PI/P
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D03PRF Example Program Results'
      P = X01AAF(XX)
      ITRACE = 0
      ITOL = 1
      ATOL(1) = 0.5D-4
      RTOL(1) = ATOL(1)
      WRITE (NOUT,99996) ATOL, NPTS
*
*     Set remesh parameters ..
*
      REMESH = .TRUE.
      NRMESH = 3
      DXMESH = 0.0D0
      CONST = 5.0D0/(NPTS-1.0D0)
      XRATIO = 1.2D0
      IPMINF = 0
      WRITE (NOUT,99999) NRMESH
*
*     Initialise mesh ..
*
      DO 20 I = 1, NPTS
         X(I) = (I-1.0D0)/(NPTS-1.0D0)
   20 CONTINUE
*
      XOUT(1) = 0.0D0
      XOUT(2) = 0.25D0
      XOUT(3) = 0.5D0
      XOUT(4) = 0.75D0
      XOUT(5) = 1.0D0
      WRITE (NOUT,99998) (XOUT(I),I=1,INTPTS)
*
      XI(1) = 0.0D0
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
         ALGOPT(6) = 2.0D0
         ALGOPT(7) = 1.0D0
      END IF
*
*     Loop over output value of t
*
      TS = 0.0D0
      TOUT = 0.0D0
*
      DO 60 IT = 1, 5
         TOUT = 0.05D0*IT
         IFAIL = 0
*
         CALL D03PRF(NPDE,TS,TOUT,PDEDEF,BNDARY,UVINIT,U,NPTS,X,NLEFT,
     +               NV,D03PEK,NXI,XI,NEQN,RTOL,ATOL,ITOL,NORM,LAOPT,
     +               ALGOPT,REMESH,NXFIX,XFIX,NRMESH,DXMESH,TRMESH,
     +               IPMINF,XRATIO,CONST,MONITF,W,NW,IW,NIW,ITASK,
     +               ITRACE,IND,IFAIL)
*
*        Interpolate at output points ..
         CALL D03PZF(NPDE,0,U,NPTS,X,XOUT,INTPTS,ITYPE,UOUT,IFAIL)
*        Check against exact solution ..
         CALL EXACT(TS,NPDE,INTPTS,XOUT,UE)
*
         WRITE (NOUT,99997) TS
         WRITE (NOUT,99994) (UOUT(1,I,1),I=1,INTPTS)
         WRITE (NOUT,99993) (UE(1,I),I=1,INTPTS)
         WRITE (NOUT,99992) (UOUT(2,I,1),I=1,INTPTS)
         WRITE (NOUT,99991) (UE(2,I),I=1,INTPTS)
*
   60 CONTINUE
      WRITE (NOUT,99995) IW(1), IW(2), IW(3), IW(5)
      STOP
*
99999 FORMAT (' Remeshing every ',I3,' time steps',/)
99998 FORMAT (' X        ',5F10.4,/)
99997 FORMAT (' T = ',F6.3)
99996 FORMAT (//'  Accuracy requirement =',D10.3,' Number of points = ',
     +       I3,/)
99995 FORMAT (' Number of integration steps in time = ',I6,/' Number o',
     +       'f function evaluations = ',I6,/' Number of Jacobian eval',
     +       'uations =',I6,/' Number of iterations = ',I6,/)
99994 FORMAT (' Approx U1',5F10.4)
99993 FORMAT (' Exact  U1',5F10.4)
99992 FORMAT (' Approx U2',5F10.4)
99991 FORMAT (' Exact  U2',5F10.4,/)
      END
*
      SUBROUTINE UVINIT(NPDE,NPTS,NXI,X,XI,U,NV,V)
*     .. Scalar Arguments ..
      INTEGER           NPDE, NPTS, NV, NXI
*     .. Array Arguments ..
      DOUBLE PRECISION  U(NPDE,NPTS), V(*), X(NPTS), XI(*)
*     .. Scalars in Common ..
      DOUBLE PRECISION  P
*     .. Local Scalars ..
      INTEGER           I
*     .. Intrinsic Functions ..
      INTRINSIC         EXP, SIN
*     .. Common blocks ..
      COMMON            /PI/P
*     .. Executable Statements ..
      DO 20 I = 1, NPTS
         U(1,I) = EXP(X(I))
         U(2,I) = X(I)**2 + SIN(2.0D0*P*X(I)**2)
   20 CONTINUE
      RETURN
      END
*
      SUBROUTINE PDEDEF(NPDE,T,X,U,UDOT,DUDX,NV,V,VDOT,RES,IRES)
*     .. Scalar Arguments ..
      DOUBLE PRECISION  T, X
      INTEGER           IRES, NPDE, NV
*     .. Array Arguments ..
      DOUBLE PRECISION  DUDX(NPDE), RES(NPDE), U(NPDE), UDOT(NPDE),
     +                  V(*), VDOT(*)
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
      SUBROUTINE BNDARY(NPDE,T,IBND,NOBC,U,UDOT,NV,V,VDOT,RES,IRES)
*     .. Scalar Arguments ..
      DOUBLE PRECISION  T
      INTEGER           IBND, IRES, NOBC, NPDE, NV
*     .. Array Arguments ..
      DOUBLE PRECISION  RES(NOBC), U(NPDE), UDOT(NPDE), V(*), VDOT(*)
*     .. Scalars in Common ..
      DOUBLE PRECISION  P
*     .. Local Scalars ..
      DOUBLE PRECISION  PP
*     .. Intrinsic Functions ..
      INTRINSIC         EXP, SIN
*     .. Common blocks ..
      COMMON            /PI/P
*     .. Executable Statements ..
      PP = 2.0D0*P
      IF (IBND.EQ.0) THEN
         IF (IRES.EQ.-1) THEN
            RES(1) = 0.0D0
         ELSE
            RES(1) = U(1) - 0.5D0*(EXP(T)+EXP(-3.0D0*T)) -
     +               0.25D0*(SIN(PP*9.0D0*T**2)-SIN(PP*T**2)) -
     +               2.0D0*T**2
         END IF
      ELSE
         IF (IRES.EQ.-1) THEN
            RES(1) = 0.0D0
         ELSE
            RES(1) = U(2) - (EXP(1.0D0-3.0D0*T)-EXP(1.0D0+T)
     +               +0.5D0*(SIN(PP*(1.0D0-3.0D0*T)**2)+SIN(PP*(1.0D0+T)
     +               **2))+1.0D0+5.0D0*T**2-2.0D0*T)
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
*     .. Scalars in Common ..
      DOUBLE PRECISION P
*     .. Local Scalars ..
      DOUBLE PRECISION PP
      INTEGER          I
*     .. Intrinsic Functions ..
      INTRINSIC        EXP, SIN
*     .. Common blocks ..
      COMMON           /PI/P
*     .. Executable Statements ..
      PP = 2.0D0*P
      DO 20 I = 1, NPTS
         U(1,I) = 0.5D0*(EXP(X(I)+T)+EXP(X(I)-3.0D0*T)) +
     +            0.25D0*(SIN(PP*(X(I)-3.0D0*T)**2)-SIN(PP*(X(I)+T)**2))
     +             + 2.0D0*T**2 - 2.0D0*X(I)*T
         U(2,I) = EXP(X(I)-3.0D0*T) - EXP(X(I)+T) + 0.5D0*(SIN(PP*(X(I)
     +            -3.0D0*T)**2)+SIN(PP*(X(I)+T)**2)) + X(I)**2 +
     +            5.0D0*T**2 - 2.0D0*X(I)*T
   20 CONTINUE
      RETURN
      END
*
      SUBROUTINE MONITF(T,NPTS,NPDE,X,U,FMON)
*     .. Scalar Arguments ..
      DOUBLE PRECISION  T
      INTEGER           NPDE, NPTS
*     .. Array Arguments ..
      DOUBLE PRECISION  FMON(NPTS), U(NPDE,NPTS), X(NPTS)
*     .. Local Scalars ..
      DOUBLE PRECISION  D2X1, D2X2, H1, H2, H3
      INTEGER           I
*     .. Intrinsic Functions ..
      INTRINSIC         ABS, MAX
*     .. Executable Statements ..
      DO 20 I = 2, NPTS - 1
         H1 = X(I) - X(I-1)
         H2 = X(I+1) - X(I)
         H3 = 0.5D0*(X(I+1)-X(I-1))
*        Second derivatives ..
         D2X1 = ABS(((U(1,I+1)-U(1,I))/H2-(U(1,I)-U(1,I-1))/H1)/H3)
         D2X2 = ABS(((U(2,I+1)-U(2,I))/H2-(U(2,I)-U(2,I-1))/H1)/H3)
         FMON(I) = MAX(D2X1,D2X2)
   20 CONTINUE
      FMON(1) = FMON(2)
      FMON(NPTS) = FMON(NPTS-1)
      RETURN
      END
