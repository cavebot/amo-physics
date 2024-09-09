*     D03PLF Example Program Text
*     Mark 17 Release. NAG Copyright 1995.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. External Subroutines ..
      EXTERNAL         EX1, EX2
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D03PLF Example Program Results'
      CALL EX1
      CALL EX2
      STOP
      END
*
      SUBROUTINE EX1
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          NPDE, NPTS, NCODE, NXI, NEQN, NIW, NW, OUTPTS
      PARAMETER        (NPDE=2,NPTS=141,NCODE=2,NXI=2,
     +                 NEQN=NPDE*NPTS+NCODE,NIW=15700,NW=11000,OUTPTS=8)
*     .. Scalars in Common ..
      DOUBLE PRECISION P
*     .. Local Scalars ..
      DOUBLE PRECISION TOUT, TS, XX
      INTEGER          I, IFAIL, II, IND, ITASK, ITOL, ITRACE, J, NOP
      CHARACTER        LAOPT, NORM
*     .. Local Arrays ..
      DOUBLE PRECISION ALGOPT(30), ATOL(1), RTOL(1), U(NEQN),
     +                 UE(NPDE,OUTPTS), UOUT(NPDE,OUTPTS), W(NW),
     +                 X(NPTS), XI(NXI), XOUT(OUTPTS)
      INTEGER          IW(NIW)
*     .. External Functions ..
      DOUBLE PRECISION X01AAF
      EXTERNAL         X01AAF
*     .. External Subroutines ..
      EXTERNAL         BNDRY1, D03PLF, EXACT, NMFLX1, ODEDEF, PDEDEF
*     .. Common blocks ..
      COMMON           /PI/P
*     .. Executable Statements ..
      WRITE (NOUT,*)
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Example 1'
      WRITE (NOUT,*)
*
      XX = 0.0D0
      P = X01AAF(XX)
      ITRACE = 0
      ITOL = 1
      NORM = '1'
      ATOL(1) = 0.1D-4
      RTOL(1) = 0.25D-3
      WRITE (NOUT,99995) NPTS, ATOL, RTOL
*
*     Initialise mesh ..
*
      DO 20 I = 1, NPTS
         X(I) = (I-1.0D0)/(NPTS-1.0D0)
   20 CONTINUE
      XI(1) = 0.0D0
      XI(2) = 1.0D0
*
*     Set initial values ..
      TS = 0.0D0
      CALL EXACT(TS,U,NPDE,X,NPTS)
      U(NEQN-1) = U(1) - U(2)
      U(NEQN) = U(NEQN-2) + U(NEQN-3)
*
      LAOPT = 'S'
      IND = 0
      ITASK = 1
*
      DO 40 I = 1, 30
         ALGOPT(I) = 0.0D0
   40 CONTINUE
*     Theta integration
      ALGOPT(1) = 1.0D0
*     Sparse matrix algebra parameters
      ALGOPT(29) = 0.1D0
      ALGOPT(30) = 1.1D0
*
      TOUT = 0.5D0
      IFAIL = 0
*
      CALL D03PLF(NPDE,TS,TOUT,PDEDEF,NMFLX1,BNDRY1,U,NPTS,X,NCODE,
     +            ODEDEF,NXI,XI,NEQN,RTOL,ATOL,ITOL,NORM,LAOPT,ALGOPT,W,
     +            NW,IW,NIW,ITASK,ITRACE,IND,IFAIL)
*
*     Set output points ..
      NOP = 0
      DO 60 I = 1, NPTS, 20
         NOP = NOP + 1
         XOUT(NOP) = X(I)
   60 CONTINUE
*
      WRITE (NOUT,99996) TS
      WRITE (NOUT,99999)
*
      DO 80 I = 1, NOP
         II = 1 + 20*(I-1)
         J = NPDE*(II-1)
         UOUT(1,I) = U(J+1)
         UOUT(2,I) = U(J+2)
   80 CONTINUE
*
*     Check against exact solution ..
      CALL EXACT(TOUT,UE,NPDE,XOUT,NOP)
      DO 100 I = 1, NOP
         WRITE (NOUT,99998) XOUT(I), UOUT(1,I), UE(1,I), UOUT(2,I),
     +     UE(2,I)
  100 CONTINUE
      WRITE (NOUT,99997)
*
      WRITE (NOUT,99994) IW(1), IW(2), IW(3), IW(5)
      RETURN
*
99999 FORMAT (8X,'X',8X,'Approx U1',3X,'Exact U1',4X,'Approx U2',3X,
     +       'Exact U2',/)
99998 FORMAT (5(3X,F9.4))
99997 FORMAT (1X,D10.4,4(2X,D12.4))
99996 FORMAT (' T = ',F6.3)
99995 FORMAT (/' NPTS = ',I4,' ATOL = ',D10.3,' RTOL = ',D10.3,/)
99994 FORMAT (' Number of integration steps in time = ',I6,/' Number ',
     +       'of function evaluations = ',I6,/' Number of Jacobian ',
     +       'evaluations =',I6,/' Number of iterations = ',I6,/)
      END
*
      SUBROUTINE PDEDEF(NPDE,T,X,U,UX,NCODE,V,VDOT,P,C,D,S,IRES)
*     .. Scalar Arguments ..
      DOUBLE PRECISION  T, X
      INTEGER           IRES, NCODE, NPDE
*     .. Array Arguments ..
      DOUBLE PRECISION  C(NPDE), D(NPDE), P(NPDE,NPDE), S(NPDE),
     +                  U(NPDE), UX(NPDE), V(*), VDOT(*)
*     .. Local Scalars ..
      INTEGER           I, J
*     .. Executable Statements ..
      DO 40 I = 1, NPDE
         C(I) = 1.0D0
         D(I) = 0.0D0
         S(I) = 0.0D0
         DO 20 J = 1, NPDE
            IF (I.EQ.J) THEN
               P(I,J) = 1.0D0
            ELSE
               P(I,J) = 0.0D0
            END IF
   20    CONTINUE
   40 CONTINUE
      RETURN
      END
*
      SUBROUTINE BNDRY1(NPDE,NPTS,T,X,U,NCODE,V,VDOT,IBND,G,IRES)
*     .. Scalar Arguments ..
      DOUBLE PRECISION  T
      INTEGER           IBND, IRES, NCODE, NPDE, NPTS
*     .. Array Arguments ..
      DOUBLE PRECISION  G(NPDE), U(NPDE,NPTS), V(*), VDOT(*), X(NPTS)
*     .. Local Scalars ..
      DOUBLE PRECISION  DUDX
*     .. Local Arrays ..
      DOUBLE PRECISION  UE(2,1)
*     .. External Subroutines ..
      EXTERNAL          EXACT
*     .. Executable Statements ..
      IF (IBND.EQ.0) THEN
         CALL EXACT(T,UE,NPDE,X(1),1)
         G(1) = U(1,1) + U(2,1) - UE(1,1) - UE(2,1)
         DUDX = (U(1,2)-U(2,2)-U(1,1)+U(2,1))/(X(2)-X(1))
         G(2) = VDOT(1) - DUDX
      ELSE
         CALL EXACT(T,UE,NPDE,X(NPTS),1)
         G(1) = U(1,NPTS) - U(2,NPTS) - UE(1,1) + UE(2,1)
         DUDX = (U(1,NPTS)+U(2,NPTS)-U(1,NPTS-1)-U(2,NPTS-1))/(X(NPTS)
     +          -X(NPTS-1))
         G(2) = VDOT(2) + 3.0D0*DUDX
      END IF
      RETURN
      END
*
      SUBROUTINE NMFLX1(NPDE,T,X,NCODE,V,ULEFT,URIGHT,FLUX,IRES)
*     .. Scalar Arguments ..
      DOUBLE PRECISION  T, X
      INTEGER           IRES, NCODE, NPDE
*     .. Array Arguments ..
      DOUBLE PRECISION  FLUX(NPDE), ULEFT(NPDE), URIGHT(NPDE), V(*)
*     .. Executable Statements ..
      FLUX(1) = 0.5D0*(3.0D0*ULEFT(1)-URIGHT(1)+3.0D0*ULEFT(2)+URIGHT(2)
     +          )
      FLUX(2) = 0.5D0*(3.0D0*ULEFT(1)+URIGHT(1)+3.0D0*ULEFT(2)-URIGHT(2)
     +          )
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
         F(1) = 0.0D0
         F(2) = 0.0D0
      ELSE
         F(1) = V(1) - UCP(1,1) + UCP(2,1)
         F(2) = V(2) - UCP(1,2) - UCP(2,2)
      END IF
      RETURN
      END
*
      SUBROUTINE EXACT(T,U,NPDE,X,NPTS)
*     Exact solution (for comparison and b.c. purposes)
*     .. Scalar Arguments ..
      DOUBLE PRECISION T
      INTEGER          NPDE, NPTS
*     .. Array Arguments ..
      DOUBLE PRECISION U(NPDE,*), X(*)
*     .. Scalars in Common ..
      DOUBLE PRECISION P
*     .. Local Scalars ..
      DOUBLE PRECISION F, G
      INTEGER          I
*     .. Intrinsic Functions ..
      INTRINSIC        COS, EXP, SIN
*     .. Common blocks ..
      COMMON           /PI/P
*     .. Executable Statements ..
      DO 20 I = 1, NPTS
         F = EXP(P*(X(I)-3.0D0*T))*SIN(2.0D0*P*(X(I)-3.0D0*T))
         G = EXP(-2.0D0*P*(X(I)+T))*COS(2.0D0*P*(X(I)+T))
         U(1,I) = F + G
         U(2,I) = F - G
   20 CONTINUE
      RETURN
      END
*
      SUBROUTINE EX2
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NPDE, NPTS, NCODE, NXI, NEQN, NIW, NWKRES,
     +                 LENODE, MLU, NW
      PARAMETER        (NPDE=3,NPTS=141,NCODE=0,NXI=0,
     +                 NEQN=NPDE*NPTS+NCODE,NIW=NEQN+24,
     +                 NWKRES=NPDE*(2*NPTS+3*NPDE+32)+7*NPTS+4,
     +                 LENODE=9*NEQN+50,MLU=3*NPDE-1,NW=(3*MLU+1)
     +                 *NEQN+NWKRES+LENODE)
*     .. Scalars in Common ..
      DOUBLE PRECISION EL0, ER0, GAMMA, RL0, RR0
*     .. Local Scalars ..
      DOUBLE PRECISION D, P, TOUT, TS, V
      INTEGER          I, IFAIL, IND, IT, ITASK, ITOL, ITRACE, K
      CHARACTER        LAOPT, NORM
*     .. Local Arrays ..
      DOUBLE PRECISION ALGOPT(30), ATOL(1), RTOL(1), U(NPDE,NPTS),
     +                 UE(3,8), W(NW), X(NPTS), XI(1)
      INTEGER          IW(NIW)
*     .. External Subroutines ..
      EXTERNAL         BNDRY2, D03PEK, D03PLF, D03PLP, NMFLX2, UVINIT
*     .. Common blocks ..
      COMMON           /INIT/EL0, ER0, RL0, RR0
      COMMON           /PARAMS/GAMMA
*     .. Executable Statements ..
      WRITE (NOUT,*)
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Example 2'
      WRITE (NOUT,*)
*     Skip heading in data file
      READ (NIN,*)
*
*     Problem parameters
      GAMMA = 1.4D0
      EL0 = 2.5D0
      ER0 = 0.25D0
      RL0 = 1.0D0
      RR0 = 0.125D0
      ITRACE = 0
      ITOL = 1
      NORM = '2'
      ATOL(1) = 0.5D-2
      RTOL(1) = 0.5D-3
      WRITE (NOUT,99994) GAMMA, EL0, ER0, RL0, RR0
      WRITE (NOUT,99996) NPTS, ATOL, RTOL
*
*     Initialise mesh
*
      DO 20 I = 1, NPTS
         X(I) = 1.0D0*(I-1.0D0)/(NPTS-1.0D0)
   20 CONTINUE
*
*     Initial values of variables
      CALL UVINIT(NPDE,NPTS,X,U)
*
      XI(1) = 0.0D0
      LAOPT = 'B'
      IND = 0
      ITASK = 1
*
      DO 40 I = 1, 30
         ALGOPT(I) = 0.0D0
   40 CONTINUE
*     Theta integration
      ALGOPT(1) = 2.0D0
      ALGOPT(6) = 2.0D0
      ALGOPT(7) = 2.0D0
*     Max. time step
      ALGOPT(13) = 0.5D-2
*
      TS = 0.0D0
      WRITE (NOUT,99998)
      DO 100 IT = 1, 2
         TOUT = IT*0.1D0
         IFAIL = 0
*
         CALL D03PLF(NPDE,TS,TOUT,D03PLP,NMFLX2,BNDRY2,U,NPTS,X,NCODE,
     +               D03PEK,NXI,XI,NEQN,RTOL,ATOL,ITOL,NORM,LAOPT,
     +               ALGOPT,W,NW,IW,NIW,ITASK,ITRACE,IND,IFAIL)
*
         WRITE (NOUT,99997) TS
*
*       Read exact data at output points ..
         READ (NIN,*)
         DO 60 I = 1, 8
            READ (NIN,99999) UE(1,I), UE(2,I), UE(3,I)
   60    CONTINUE
*
*       Calculate density, velocity and pressure ..
         K = 0
         DO 80 I = 29, NPTS - 14, 14
            D = U(1,I)
            V = U(2,I)/D
            P = D*(GAMMA-1.0D0)*(U(3,I)/D-0.5D0*V**2)
            K = K + 1
            WRITE (NOUT,99993) X(I), D, UE(1,K), V, UE(2,K), P,
     +        UE(3,K)
   80    CONTINUE
  100 CONTINUE
*
      WRITE (NOUT,99995) IW(1), IW(2), IW(3), IW(5)
      RETURN
*
99999 FORMAT (3(1X,F6.4))
99998 FORMAT (4X,'X',4X,'APPROX D',1X,'EXACT D',2X,'APPROX V',1X,'EXAC',
     +       'T V',2X,'APPROX P',1X,'EXACT P')
99997 FORMAT (/' T = ',F6.3,/)
99996 FORMAT (/' NPTS = ',I4,' ATOL = ',D10.3,' RTOL = ',D10.3,/)
99995 FORMAT (/' Number of integration steps in time = ',I6,/' Number ',
     +       'of function evaluations = ',I6,/' Number of Jacobian ',
     +       'evaluations =',I6,/' Number of iterations = ',I6,/)
99994 FORMAT (/' GAMMA =',F6.3,'  EL0 =',F6.3,'  ER0 =',F6.3,'  RL0 =',
     +       F6.3,'  RR0 =',F6.3)
99993 FORMAT (1X,F6.4,6(3X,F6.4))
      END
*
      SUBROUTINE UVINIT(NPDE,NPTS,X,U)
*     .. Scalar Arguments ..
      INTEGER           NPDE, NPTS
*     .. Array Arguments ..
      DOUBLE PRECISION  U(NPDE,NPTS), X(NPTS)
*     .. Scalars in Common ..
      DOUBLE PRECISION  EL0, ER0, RL0, RR0
*     .. Local Scalars ..
      INTEGER           I
*     .. Common blocks ..
      COMMON            /INIT/EL0, ER0, RL0, RR0
*     .. Executable Statements ..
      DO 20 I = 1, NPTS
         IF (X(I).LT.0.5D0) THEN
            U(1,I) = RL0
            U(2,I) = 0.0D0
            U(3,I) = EL0
         ELSE IF (X(I).EQ.0.5D0) THEN
            U(1,I) = 0.5D0*(RL0+RR0)
            U(2,I) = 0.0D0
            U(3,I) = 0.5D0*(EL0+ER0)
         ELSE
            U(1,I) = RR0
            U(2,I) = 0.0D0
            U(3,I) = ER0
         END IF
   20 CONTINUE
      RETURN
      END
*
      SUBROUTINE BNDRY2(NPDE,NPTS,T,X,U,NCODE,V,VDOT,IBND,G,IRES)
*     .. Scalar Arguments ..
      DOUBLE PRECISION  T
      INTEGER           IBND, IRES, NCODE, NPDE, NPTS
*     .. Array Arguments ..
      DOUBLE PRECISION  G(NPDE), U(NPDE,NPTS), V(*), VDOT(*), X(NPTS)
*     .. Scalars in Common ..
      DOUBLE PRECISION  EL0, ER0, RL0, RR0
*     .. Common blocks ..
      COMMON            /INIT/EL0, ER0, RL0, RR0
*     .. Executable Statements ..
      IF (IBND.EQ.0) THEN
         G(1) = U(1,1) - RL0
         G(2) = U(2,1)
         G(3) = U(3,1) - EL0
      ELSE
         G(1) = U(1,NPTS) - RR0
         G(2) = U(2,NPTS)
         G(3) = U(3,NPTS) - ER0
      END IF
      RETURN
      END
*
      SUBROUTINE NMFLX2(NPDE,T,X,NCODE,V,ULEFT,URIGHT,FLUX,IRES)
*     .. Scalar Arguments ..
      DOUBLE PRECISION  T, X
      INTEGER           IRES, NCODE, NPDE
*     .. Array Arguments ..
      DOUBLE PRECISION  FLUX(NPDE), ULEFT(NPDE), URIGHT(NPDE), V(*)
*     .. Scalars in Common ..
      DOUBLE PRECISION  GAMMA
*     .. Local Scalars ..
      INTEGER           IFAIL
      CHARACTER         PATH, SOLVER
*     .. External Subroutines ..
      EXTERNAL          D03PUF, D03PVF
*     .. Common blocks ..
      COMMON            /PARAMS/GAMMA
*     .. Save statement ..
      SAVE              /PARAMS/
*     .. Executable Statements ..
      IFAIL = 0
      SOLVER = 'R'
      IF (SOLVER.EQ.'R') THEN
*       ROE SCHEME ..
         CALL D03PUF(ULEFT,URIGHT,GAMMA,FLUX,IFAIL)
      ELSE
*       OSHER SCHEME ..
         PATH = 'P'
         CALL D03PVF(ULEFT,URIGHT,GAMMA,PATH,FLUX,IFAIL)
      END IF
      RETURN
      END
