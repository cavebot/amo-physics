*     D03PPF Example Program Text
*     Mark 16 Release. NAG Copyright 1993.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          NPDE, NPTS, NCODE, M, NXI, NXFIX, NEQN, NIW,
     +                 NWKRES, LENODE, NW, INTPTS, ITYPE
      PARAMETER        (NPDE=1,NPTS=61,NCODE=0,M=0,NXI=0,NXFIX=0,
     +                 NEQN=NPDE*NPTS+NCODE,NIW=25+NXFIX,
     +                 NWKRES=NPDE*(NPTS+3*NPDE+21)+7*NPTS+NXFIX+3,
     +                 LENODE=11*NEQN+50,NW=NEQN*NEQN+NEQN+NWKRES+
     +                 LENODE,INTPTS=5,ITYPE=1)
*     .. Scalars in Common ..
      DOUBLE PRECISION E
*     .. Local Scalars ..
      DOUBLE PRECISION CONST, DXMESH, TOUT, TRMESH, TS, XRATIO
      INTEGER          I, IFAIL, IND, IPMINF, IT, ITASK, ITOL, ITRACE,
     +                 NRMESH
      LOGICAL          REMESH, THETA
      CHARACTER        LAOPT, NORM
*     .. Local Arrays ..
      DOUBLE PRECISION ALGOPT(30), ATOL(1), RTOL(1), U(NEQN),
     +                 UE(INTPTS), UOUT(NPDE,INTPTS,ITYPE), W(NW),
     +                 X(NPTS), XFIX(1), XI(1), XOUT(INTPTS)
      INTEGER          IW(NIW)
*     .. External Subroutines ..
      EXTERNAL         BNDARY, D03PCK, D03PPF, D03PZF, EXACT, MONITF,
     +                 PDEDEF, UVINIT
*     .. Common blocks ..
      COMMON           /EPS/E
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D03PPF Example Program Results'
      E = 0.005D0
      ITRACE = 0
      ITOL = 1
      ATOL(1) = 0.5D-4
      RTOL(1) = ATOL(1)
      WRITE (NOUT,99998) ATOL, NPTS
*
*     Initialise mesh ..
*
      DO 20 I = 1, NPTS
         X(I) = (I-1.0D0)/(NPTS-1.0D0)
   20 CONTINUE
*
*     Set remesh parameters..
*
      REMESH = .TRUE.
      NRMESH = 3
      DXMESH = 0.5D0
      CONST = 2.0D0/(NPTS-1.0D0)
      XRATIO = 1.5D0
      IPMINF = 0
*
      WRITE (NOUT,99993) NRMESH
      WRITE (NOUT,99992) E
      WRITE (NOUT,*)
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
      ELSE
         ALGOPT(1) = 0.0D0
      END IF
*
*     Loop over output value of t
*
      TS = 0.0D0
      TOUT = 0.0D0
      DO 60 IT = 1, 5
         TOUT = 0.2D0*IT
         IFAIL = 0
*
         CALL D03PPF(NPDE,M,TS,TOUT,PDEDEF,BNDARY,UVINIT,U,NPTS,X,NCODE,
     +               D03PCK,NXI,XI,NEQN,RTOL,ATOL,ITOL,NORM,LAOPT,
     +               ALGOPT,REMESH,NXFIX,XFIX,NRMESH,DXMESH,TRMESH,
     +               IPMINF,XRATIO,CONST,MONITF,W,NW,IW,NIW,ITASK,
     +               ITRACE,IND,IFAIL)
*
*        Set output points ..
         IF (IT.EQ.1) THEN
            XOUT(1) = 0.3D0
            XOUT(2) = 0.4D0
            XOUT(3) = 0.5D0
            XOUT(4) = 0.6D0
            XOUT(5) = 0.7D0
         ELSE IF (IT.EQ.2) THEN
            XOUT(1) = 0.4D0
            XOUT(2) = 0.5D0
            XOUT(3) = 0.6D0
            XOUT(4) = 0.7D0
            XOUT(5) = 0.8D0
         ELSE IF (IT.EQ.3) THEN
            XOUT(1) = 0.6D0
            XOUT(2) = 0.65D0
            XOUT(3) = 0.7D0
            XOUT(4) = 0.75D0
            XOUT(5) = 0.8D0
         ELSE IF (IT.EQ.4) THEN
            XOUT(1) = 0.7D0
            XOUT(2) = 0.75D0
            XOUT(3) = 0.8D0
            XOUT(4) = 0.85D0
            XOUT(5) = 0.9D0
         ELSE IF (IT.EQ.5) THEN
            XOUT(1) = 0.8D0
            XOUT(2) = 0.85D0
            XOUT(3) = 0.9D0
            XOUT(4) = 0.95D0
            XOUT(5) = 1.0D0
         END IF
*
         WRITE (NOUT,99999) TS
         WRITE (NOUT,99996) (XOUT(I),I=1,INTPTS)
*        Interpolate at output points ..
         CALL D03PZF(NPDE,M,U,NPTS,X,XOUT,INTPTS,ITYPE,UOUT,IFAIL)
*
*        Check against exact solution ..
         CALL EXACT(TS,XOUT,INTPTS,UE)
*
         WRITE (NOUT,99995) (UOUT(1,I,1),I=1,INTPTS)
         WRITE (NOUT,99994) (UE(I),I=1,INTPTS)
*
   60 CONTINUE
      WRITE (NOUT,99997) IW(1), IW(2), IW(3), IW(5)
      STOP
*
99999 FORMAT (' T = ',F6.3)
99998 FORMAT (//'  Accuracy requirement =',D10.3,' Number of points = ',
     +       I3,/)
99997 FORMAT (' Number of integration steps in time = ',I6,/' Number o',
     +       'f function evaluations = ',I6,/' Number of Jacobian eval',
     +       'uations =',I6,/' Number of iterations = ',I6,/)
99996 FORMAT (1X,'X           ',5F9.4)
99995 FORMAT (1X,'Approx sol. ',5F9.4)
99994 FORMAT (1X,'Exact  sol. ',5F9.4,/)
99993 FORMAT (2X,'Remeshing every',I3,' time steps',/)
99992 FORMAT (2X,'E =',F8.3)
      END
*
      SUBROUTINE UVINIT(NPDE,NPTS,NXI,X,XI,U,NCODE,V)
*     .. Scalar Arguments ..
      INTEGER           NCODE, NPDE, NPTS, NXI
*     .. Array Arguments ..
      DOUBLE PRECISION  U(NPDE,NPTS), V(*), X(NPTS), XI(*)
*     .. Scalars in Common ..
      DOUBLE PRECISION  E
*     .. Local Scalars ..
      DOUBLE PRECISION  A, B, C, T
      INTEGER           I
*     .. Intrinsic Functions ..
      INTRINSIC         EXP
*     .. Common blocks ..
      COMMON            /EPS/E
*     .. Executable Statements ..
      T = 0.0D0
      DO 20 I = 1, NPTS
         A = (X(I)-0.25D0-0.75D0*T)/(4.0D0*E)
         B = (0.9D0*X(I)-0.325D0-0.495D0*T)/(2.0D0*E)
         IF (A.GT.0.0D0 .AND. A.GT.B) THEN
            A = EXP(-A)
            C = (0.8D0*X(I)-0.4D0-0.24D0*T)/(4.0D0*E)
            C = EXP(C)
            U(1,I) = (0.5D0+0.1D0*C+A)/(1.0D0+C+A)
         ELSE IF (B.GT.0.0D0 .AND. B.GE.A) THEN
            B = EXP(-B)
            C = (-0.8D0*X(I)+0.4D0+0.24D0*T)/(4.0D0*E)
            C = EXP(C)
            U(1,I) = (0.1D0+0.5D0*C+B)/(1.0D0+C+B)
         ELSE
            A = EXP(A)
            B = EXP(B)
            U(1,I) = (1.0D0+0.5D0*A+0.1D0*B)/(1.0D0+A+B)
         END IF
   20 CONTINUE
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
*     .. Scalars in Common ..
      DOUBLE PRECISION  E
*     .. Common blocks ..
      COMMON            /EPS/E
*     .. Executable Statements ..
      P(1,1) = 1.0D0
      R(1) = E*UX(1)
      Q(1) = U(1)*UX(1)
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
*     .. Scalars in Common ..
      DOUBLE PRECISION  E
*     .. Local Scalars ..
      DOUBLE PRECISION  A, B, C, UE, X
*     .. Intrinsic Functions ..
      INTRINSIC         EXP
*     .. Common blocks ..
      COMMON            /EPS/E
*     .. Executable Statements ..
      BETA(1) = 0.0D0
      IF (IBND.EQ.0) THEN
         X = 0.0D0
         A = (X-0.25D0-0.75D0*T)/(4.0D0*E)
         B = (0.9D0*X-0.325D0-0.495D0*T)/(2.0D0*E)
         IF (A.GT.0.0D0 .AND. A.GT.B) THEN
            A = EXP(-A)
            C = (0.8D0*X-0.4D0-0.24D0*T)/(4.0D0*E)
            C = EXP(C)
            UE = (0.5D0+0.1D0*C+A)/(1.0D0+C+A)
         ELSE IF (B.GT.0.0D0 .AND. B.GE.A) THEN
            B = EXP(-B)
            C = (-0.8D0*X+0.4D0+0.24D0*T)/(4.0D0*E)
            C = EXP(C)
            UE = (0.1D0+0.5D0*C+B)/(1.0D0+C+B)
         ELSE
            A = EXP(A)
            B = EXP(B)
            UE = (1.0D0+0.5D0*A+0.1D0*B)/(1.0D0+A+B)
         END IF
      ELSE
         X = 1.0D0
         A = (X-0.25D0-0.75D0*T)/(4.0D0*E)
         B = (0.9D0*X-0.325D0-0.495D0*T)/(2.0D0*E)
         IF (A.GT.0.0D0 .AND. A.GT.B) THEN
            A = EXP(-A)
            C = (0.8D0*X-0.4D0-0.24D0*T)/(4.0D0*E)
            C = EXP(C)
            UE = (0.5D0+0.1D0*C+A)/(1.0D0+C+A)
         ELSE IF (B.GT.0.0D0 .AND. B.GE.A) THEN
            B = EXP(-B)
            C = (-0.8D0*X+0.4D0+0.24D0*T)/(4.0D0*E)
            C = EXP(C)
            UE = (0.1D0+0.5D0*C+B)/(1.0D0+C+B)
         ELSE
            A = EXP(A)
            B = EXP(B)
            UE = (1.0D0+0.5D0*A+0.1D0*B)/(1.0D0+A+B)
         END IF
      END IF
      GAMMA(1) = U(1) - UE
      RETURN
      END
*
      SUBROUTINE EXACT(T,X,NPTS,U)
*     Exact solution (for comparison purposes)
*     .. Scalar Arguments ..
      DOUBLE PRECISION T
      INTEGER          NPTS
*     .. Array Arguments ..
      DOUBLE PRECISION U(NPTS), X(NPTS)
*     .. Scalars in Common ..
      DOUBLE PRECISION E
*     .. Local Scalars ..
      DOUBLE PRECISION A, B, C
      INTEGER          I
*     .. Intrinsic Functions ..
      INTRINSIC        EXP
*     .. Common blocks ..
      COMMON           /EPS/E
*     .. Executable Statements ..
      DO 20 I = 1, NPTS
         A = (X(I)-0.25D0-0.75D0*T)/(4.0D0*E)
         B = (0.9D0*X(I)-0.325D0-0.495D0*T)/(2.0D0*E)
         IF (A.GT.0.0D0 .AND. A.GT.B) THEN
            A = EXP(-A)
            C = (0.8D0*X(I)-0.4D0-0.24D0*T)/(4.0D0*E)
            C = EXP(C)
            U(I) = (0.5D0+0.1D0*C+A)/(1.0D0+C+A)
         ELSE IF (B.GT.0.0D0 .AND. B.GE.A) THEN
            B = EXP(-B)
            C = (-0.8D0*X(I)+0.4D0+0.24D0*T)/(4.0D0*E)
            C = EXP(C)
            U(I) = (0.1D0+0.5D0*C+B)/(1.0D0+C+B)
         ELSE
            A = EXP(A)
            B = EXP(B)
            U(I) = (1.0D0+0.5D0*A+0.1D0*B)/(1.0D0+A+B)
         END IF
   20 CONTINUE
      RETURN
      END
*
      SUBROUTINE MONITF(T,NPTS,NPDE,X,U,R,FMON)
*     .. Scalar Arguments ..
      DOUBLE PRECISION  T
      INTEGER           NPDE, NPTS
*     .. Array Arguments ..
      DOUBLE PRECISION  FMON(NPTS), R(NPDE,NPTS), U(NPDE,NPTS), X(NPTS)
*     .. Local Scalars ..
      DOUBLE PRECISION  DRDX, H
      INTEGER           I, K, L
*     .. Intrinsic Functions ..
      INTRINSIC         ABS, MAX, MIN
*     .. Executable Statements ..
      DO 20 I = 1, NPTS - 1
         K = MAX(1,I-1)
         L = MIN(NPTS,I+1)
         H = (X(L)-X(K))*0.5D0
*        Second derivative ..
         DRDX = (R(1,I+1)-R(1,I))/H
         FMON(I) = ABS(DRDX)
   20 CONTINUE
      FMON(NPTS) = FMON(NPTS-1)
      RETURN
      END
