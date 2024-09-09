*     D03PSF Example Program Text
*     Mark 17 Release. NAG Copyright 1995.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. External Subroutines ..
      EXTERNAL         EX1, EX2
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D03PSF Example Program Results'
      CALL EX1
      CALL EX2
      STOP
      END
*
      SUBROUTINE EX1
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          NPDE, NPTS, NCODE, NXI, NXFIX, NEQN, NIW, NWKRES,
     +                 LENODE, MLU, NW, INTPTS, ITYPE
      PARAMETER        (NPDE=1,NPTS=61,NCODE=0,NXI=0,NXFIX=0,
     +                 NEQN=NPDE*NPTS+NCODE,NIW=25+NXFIX+NEQN,
     +                 NWKRES=NPDE*(3*NPTS+3*NPDE+32)+7*NPTS+3,
     +                 LENODE=11*NEQN+50,MLU=3*NPDE-1,NW=(3*MLU+1)
     +                 *NEQN+NWKRES+LENODE,INTPTS=7,ITYPE=1)
*     .. Scalars in Common ..
      DOUBLE PRECISION P
*     .. Local Scalars ..
      DOUBLE PRECISION CONST, DXMESH, TOUT, TRMESH, TS, XRATIO, XX
      INTEGER          I, IFAIL, IND, IPMINF, IT, ITASK, ITOL, ITRACE,
     +                 M, NRMESH
      LOGICAL          REMESH
      CHARACTER        LAOPT, NORM
*     .. Local Arrays ..
      DOUBLE PRECISION ALGOPT(30), ATOL(1), RTOL(1), U(NPDE,NPTS),
     +                 UOUT(NPDE,INTPTS,ITYPE), W(NW), X(NPTS), XFIX(1),
     +                 XI(1), XOUT(INTPTS)
      INTEGER          IW(NIW)
*     .. External Functions ..
      DOUBLE PRECISION X01AAF
      EXTERNAL         X01AAF
*     .. External Subroutines ..
      EXTERNAL         BNDRY1, D03PEK, D03PSF, D03PZF, MONIT1, NMFLX1,
     +                 PDEF1, UVIN1
*     .. Common blocks ..
      COMMON           /PI/P
*     .. Data statements ..
      DATA             XOUT(1)/0.2D+0/, XOUT(2)/0.3D+0/,
     +                 XOUT(3)/0.4D+0/, XOUT(4)/0.5D+0/,
     +                 XOUT(5)/0.6D+0/, XOUT(6)/0.7D+0/, XOUT(7)/0.8D+0/
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
      ATOL(1) = 0.1D-3
      RTOL(1) = 0.1D-3
      WRITE (NOUT,99998) NPTS, ATOL, RTOL
*
*     Initialise mesh ..
*
      DO 20 I = 1, NPTS
         X(I) = (I-1.0D0)/(NPTS-1.0D0)
   20 CONTINUE
      XFIX(1) = 0.0D0
*
*     Set remesh parameters..
*
      REMESH = .TRUE.
      NRMESH = 3
      DXMESH = 0.0D0
      TRMESH = 0.0D0
      CONST = 2.0D0/(NPTS-1.0D0)
      XRATIO = 1.5D0
      IPMINF = 0
*
      XI(1) = 0.0D0
      LAOPT = 'B'
      IND = 0
      ITASK = 1
*
      DO 40 I = 1, 30
         ALGOPT(I) = 0.0D0
   40 CONTINUE
*     b.d.f. integration
      ALGOPT(1) = 1.0D0
      ALGOPT(13) = 0.5D-2
*
*     Loop over output value of t
*
      TS = 0.0D0
      TOUT = 0.0D0
      DO 60 IT = 1, 3
         TOUT = IT*0.1D0
         IFAIL = 0
*
         CALL D03PSF(NPDE,TS,TOUT,PDEF1,NMFLX1,BNDRY1,UVIN1,U,NPTS,X,
     +               NCODE,D03PEK,NXI,XI,NEQN,RTOL,ATOL,ITOL,NORM,LAOPT,
     +               ALGOPT,REMESH,NXFIX,XFIX,NRMESH,DXMESH,TRMESH,
     +               IPMINF,XRATIO,CONST,MONIT1,W,NW,IW,NIW,ITASK,
     +               ITRACE,IND,IFAIL)
*
         WRITE (NOUT,99999) TS
         WRITE (NOUT,99996) (XOUT(I),I=1,INTPTS)
*         Interpolate at output points ..
         M = 0
         CALL D03PZF(NPDE,M,U,NPTS,X,XOUT,INTPTS,ITYPE,UOUT,IFAIL)
*
         WRITE (NOUT,99995) (UOUT(1,I,1),I=1,INTPTS)
   60 CONTINUE
*
      WRITE (NOUT,99997) IW(1), IW(2), IW(3), IW(5)
      RETURN
*
99999 FORMAT (' T = ',F6.3)
99998 FORMAT (/'  NPTS = ',I4,' ATOL = ',D10.3,' RTOL = ',D10.3,/)
99997 FORMAT (' Number of integration steps in time = ',I6,/' Number ',
     +       'of function evaluations = ',I6,/' Number of Jacobian ',
     +       'evaluations =',I6,/' Number of iterations = ',I6,/)
99996 FORMAT (1X,'X        ',7F9.4)
99995 FORMAT (1X,'Approx U ',7F9.4,/)
      END
*
      SUBROUTINE UVIN1(NPDE,NPTS,NXI,X,XI,U,NCODE,V)
*     .. Scalar Arguments ..
      INTEGER           NCODE, NPDE, NPTS, NXI
*     .. Array Arguments ..
      DOUBLE PRECISION  U(NPDE,NPTS), V(*), X(NPTS), XI(*)
*     .. Scalars in Common ..
      DOUBLE PRECISION  P
*     .. Local Scalars ..
      DOUBLE PRECISION  TMP
      INTEGER           I
*     .. Intrinsic Functions ..
      INTRINSIC         SIN
*     .. Common blocks ..
      COMMON            /PI/P
*     .. Executable Statements ..
      DO 20 I = 1, NPTS
         IF (X(I).GT.0.2D0 .AND. X(I).LE.0.4D0) THEN
            TMP = P*(5.0D0*X(I)-1.0D0)
            U(1,I) = SIN(TMP)
         ELSE
            U(1,I) = 0.0D0
         END IF
   20 CONTINUE
      RETURN
      END
*
      SUBROUTINE PDEF1(NPDE,T,X,U,UX,NCODE,V,VDOT,P,C,D,S,IRES)
*     .. Scalar Arguments ..
      DOUBLE PRECISION  T, X
      INTEGER           IRES, NCODE, NPDE
*     .. Array Arguments ..
      DOUBLE PRECISION  C(NPDE), D(NPDE), P(NPDE,NPDE), S(NPDE),
     +                  U(NPDE), UX(NPDE), V(*), VDOT(*)
*     .. Executable Statements ..
      P(1,1) = 1.0D0
      C(1) = 0.2D-2
      D(1) = UX(1)
      S(1) = 0.0D0
      RETURN
      END
*
      SUBROUTINE BNDRY1(NPDE,NPTS,T,X,U,NCODE,V,VDOT,IBND,G,IRES)
*     .. Scalar Arguments ..
      DOUBLE PRECISION  T
      INTEGER           IBND, IRES, NCODE, NPDE, NPTS
*     .. Array Arguments ..
      DOUBLE PRECISION  G(NPDE), U(NPDE,NPTS), V(*), VDOT(*), X(NPTS)
*     .. Executable Statements ..
*     Zero solution at both boundaries
      IF (IBND.EQ.0) THEN
         G(1) = U(1,1)
      ELSE
         G(1) = U(1,NPTS)
      END IF
      RETURN
      END
*
      SUBROUTINE MONIT1(T,NPTS,NPDE,X,U,FMON)
*     .. Scalar Arguments ..
      DOUBLE PRECISION  T
      INTEGER           NPDE, NPTS
*     .. Array Arguments ..
      DOUBLE PRECISION  FMON(NPTS), U(NPDE,NPTS), X(NPTS)
*     .. Local Scalars ..
      DOUBLE PRECISION  H1, H2, H3
      INTEGER           I
*     .. Intrinsic Functions ..
      INTRINSIC         ABS
*     .. Executable Statements ..
*     Executable Statements ..
      DO 20 I = 2, NPTS - 1
         H1 = X(I) - X(I-1)
         H2 = X(I+1) - X(I)
         H3 = 0.5D0*(X(I+1)-X(I-1))
*       Second derivatives ..
         FMON(I) = ABS(((U(1,I+1)-U(1,I))/H2-(U(1,I)-U(1,I-1))/H1)/H3)
   20 CONTINUE
      FMON(1) = FMON(2)
      FMON(NPTS) = FMON(NPTS-1)
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
      FLUX(1) = ULEFT(1)
      RETURN
      END
*
      SUBROUTINE EX2
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          NPDE, NPTS, NCODE, NXI, NXFIX, NEQN, NIW, NWKRES,
     +                 LENODE, MLU, NW, INTPTS, ITYPE
      PARAMETER        (NPDE=1,NPTS=61,NCODE=0,NXI=0,NXFIX=0,
     +                 NEQN=NPDE*NPTS+NCODE,NIW=25+NXFIX+NEQN,
     +                 NWKRES=NPDE*(2*NPTS+3*NPDE+32)+7*NPTS+3,
     +                 LENODE=11*NEQN+50,MLU=3*NPDE-1,NW=(3*MLU+1)
     +                 *NEQN+NWKRES+LENODE,INTPTS=7,ITYPE=1)
*     .. Local Scalars ..
      DOUBLE PRECISION CONST, DXMESH, TOUT, TRMESH, TS, XRATIO
      INTEGER          I, IFAIL, IND, IPMINF, IT, ITASK, ITOL, ITRACE,
     +                 M, NRMESH
      LOGICAL          REMESH
      CHARACTER        LAOPT, NORM
*     .. Local Arrays ..
      DOUBLE PRECISION ALGOPT(30), ATOL(1), RTOL(1), U(NEQN),
     +                 UE(1,INTPTS), UOUT(1,INTPTS,ITYPE), W(NW),
     +                 X(NPTS), XFIX(1), XI(1), XOUT(INTPTS)
      INTEGER          IW(NIW)
*     .. External Subroutines ..
      EXTERNAL         BNDRY2, D03PEK, D03PSF, D03PZF, EXACT, MONIT2,
     +                 NMFLX2, PDEF2, UVIN2
*     .. Data statements ..
      DATA             XOUT(1)/0.0D+0/, XOUT(2)/0.3D+0/,
     +                 XOUT(3)/0.4D+0/, XOUT(4)/0.5D+0/,
     +                 XOUT(5)/0.6D+0/, XOUT(6)/0.7D+0/, XOUT(7)/1.0D+0/
*     .. Executable Statements ..
      WRITE (NOUT,*)
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Example 2'
      WRITE (NOUT,*)
*
      ITRACE = 0
      ITOL = 1
      NORM = '1'
      ATOL(1) = 0.5D-3
      RTOL(1) = 0.5D-1
      WRITE (NOUT,99998) NPTS, ATOL, RTOL
*
*     Initialise mesh ..
*
      DO 20 I = 1, NPTS
         X(I) = (I-1.0D0)/(NPTS-1.0D0)
   20 CONTINUE
      XFIX(1) = 0.0D0
*
*     Set remesh parameters..
*
      REMESH = .TRUE.
      NRMESH = 5
      DXMESH = 0.0D0
      CONST = 1.0D0/(NPTS-1.0D0)
      XRATIO = 1.5D0
      IPMINF = 0
*
      XI(1) = 0.0D0
      LAOPT = 'B'
      IND = 0
      ITASK = 1
*
      DO 40 I = 1, 30
         ALGOPT(I) = 0.0D0
   40 CONTINUE
*     Theta integration ..
      ALGOPT(1) = 2.0D0
      ALGOPT(6) = 2.0D0
      ALGOPT(7) = 2.0D0
*     Max. time step ..
      ALGOPT(13) = 2.5D-3
*
      TS = 0.0D0
      TOUT = 0.0D0
      IFAIL = 0
      DO 80 IT = 1, 2
         TOUT = IT*0.2D0
         CALL D03PSF(NPDE,TS,TOUT,PDEF2,NMFLX2,BNDRY2,UVIN2,U,NPTS,X,
     +               NCODE,D03PEK,NXI,XI,NEQN,RTOL,ATOL,ITOL,NORM,LAOPT,
     +               ALGOPT,REMESH,NXFIX,XFIX,NRMESH,DXMESH,TRMESH,
     +               IPMINF,XRATIO,CONST,MONIT2,W,NW,IW,NIW,ITASK,
     +               ITRACE,IND,IFAIL)
*
         WRITE (NOUT,99999) TS
         WRITE (NOUT,99996)
*        Interpolate at output points ..
         M = 0
         CALL D03PZF(NPDE,M,U,NPTS,X,XOUT,INTPTS,ITYPE,UOUT,IFAIL)
*
*        Check against exact solution ..
         CALL EXACT(TOUT,UE,XOUT,INTPTS)
         DO 60 I = 1, INTPTS
            WRITE (NOUT,99995) XOUT(I), UOUT(1,I,1), UE(1,I)
   60    CONTINUE
   80 CONTINUE
*
      WRITE (NOUT,99997) IW(1), IW(2), IW(3), IW(5)
      RETURN
*
99999 FORMAT (' T = ',F6.3)
99998 FORMAT (/' NPTS = ',I4,' ATOL = ',D10.3,' RTOL = ',D10.3,/)
99997 FORMAT (/' Number of integration steps in time = ',I6,/' Number ',
     +       'of function evaluations = ',I6,/' Number of Jacobian ',
     +       'evaluations =',I6,/' Number of iterations = ',I6,/)
99996 FORMAT (8X,'X',8X,'Approx U',4X,'Exact U',/)
99995 FORMAT (3(3X,F9.4))
      END
*
      SUBROUTINE UVIN2(NPDE,NPTS,NXI,X,XI,U,NCODE,V)
*     .. Scalar Arguments ..
      INTEGER           NCODE, NPDE, NPTS, NXI
*     .. Array Arguments ..
      DOUBLE PRECISION  U(NPDE,NPTS), V(*), X(NPTS), XI(*)
*     .. Local Scalars ..
      DOUBLE PRECISION  T
*     .. External Subroutines ..
      EXTERNAL          EXACT
*     .. Executable Statements ..
      T = 0.0D0
      CALL EXACT(T,U,X,NPTS)
      RETURN
      END
*
      SUBROUTINE PDEF2(NPDE,T,X,U,UX,NCODE,V,VDOT,P,C,D,S,IRES)
*     .. Scalar Arguments ..
      DOUBLE PRECISION  T, X
      INTEGER           IRES, NCODE, NPDE
*     .. Array Arguments ..
      DOUBLE PRECISION  C(NPDE), D(NPDE), P(NPDE,NPDE), S(NPDE),
     +                  U(NPDE), UX(NPDE), V(*), VDOT(*)
*     .. Executable Statements ..
      P(1,1) = 1.0D0
      C(1) = 0.0D0
      D(1) = 0.0D0
      S(1) = -1.0D2*U(1)*(U(1)-1.0D0)*(U(1)-0.5D0)
      RETURN
      END
*
      SUBROUTINE BNDRY2(NPDE,NPTS,T,X,U,NCODE,V,VDOT,IBND,G,IRES)
*     .. Scalar Arguments ..
      DOUBLE PRECISION  T
      INTEGER           IBND, IRES, NCODE, NPDE, NPTS
*     .. Array Arguments ..
      DOUBLE PRECISION  G(NPDE), U(NPDE,NPTS), V(*), VDOT(*), X(NPTS)
*     .. Local Arrays ..
      DOUBLE PRECISION  UE(1,1)
*     .. External Subroutines ..
      EXTERNAL          EXACT
*     .. Executable Statements ..
*     Solution known to be constant at both boundaries ..
      IF (IBND.EQ.0) THEN
         CALL EXACT(T,UE,X(1),1)
         G(1) = UE(1,1) - U(1,1)
      ELSE
         CALL EXACT(T,UE,X(NPTS),1)
         G(1) = UE(1,1) - U(1,NPTS)
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
*     .. Executable Statements ..
      FLUX(1) = ULEFT(1)
      RETURN
      END
*
      SUBROUTINE MONIT2(T,NPTS,NPDE,X,U,FMON)
*     .. Scalar Arguments ..
      DOUBLE PRECISION  T
      INTEGER           NPDE, NPTS
*     .. Array Arguments ..
      DOUBLE PRECISION  FMON(NPTS), U(NPDE,NPTS), X(NPTS)
*     .. Scalars in Common ..
      DOUBLE PRECISION  XA
      INTEGER           ICOUNT
*     .. Local Scalars ..
      DOUBLE PRECISION  H1, PI, UX, UXMAX, XL, XLEFT, XMAX, XR, XRIGHT,
     +                  XX
      INTEGER           I
*     .. External Functions ..
      DOUBLE PRECISION  X01AAF
      EXTERNAL          X01AAF
*     .. Intrinsic Functions ..
      INTRINSIC         ABS, COS
*     .. Common blocks ..
      COMMON            /SAVER/XA, ICOUNT
*     .. Save statement ..
      SAVE              /SAVER/
*     .. Executable Statements ..
      ICOUNT = ICOUNT + 1
      XX = 0.0D0
      PI = X01AAF(XX)
*     Locate shock ..
      UXMAX = 0.0D0
      XMAX = 0.0D0
      DO 20 I = 2, NPTS - 1
         H1 = X(I) - X(I-1)
         UX = ABS((U(1,I)-U(1,I-1))/H1)
         IF (UX.GT.UXMAX) THEN
            UXMAX = UX
            XMAX = X(I)
         END IF
   20 CONTINUE
*     Assign width (on first call only) ..
      IF (ICOUNT.EQ.1) THEN
         XLEFT = XMAX - X(1)
         XRIGHT = X(NPTS) - XMAX
         IF (XLEFT.GT.XRIGHT) THEN
            XA = XRIGHT
         ELSE
            XA = XLEFT
         END IF
      END IF
      XL = XMAX - XA
      XR = XMAX + XA
*     Assign monitor function ..
      DO 40 I = 1, NPTS
         IF (X(I).GT.XL .AND. X(I).LT.XR) THEN
            FMON(I) = 1.0D0 + COS(PI*(X(I)-XMAX)/XA)
         ELSE
            FMON(I) = 0.0D0
         END IF
   40 CONTINUE
      RETURN
      END
*
      SUBROUTINE EXACT(T,U,X,NPTS)
*     Exact solution (for comparison and b.c. purposes)
*     .. Scalar Arguments ..
      DOUBLE PRECISION T
      INTEGER          NPTS
*     .. Array Arguments ..
      DOUBLE PRECISION U(1,NPTS), X(*)
*     .. Local Scalars ..
      DOUBLE PRECISION DEL, PSI, RM, RN, S
      INTEGER          I
*     .. Executable Statements ..
      S = 0.1D0
      DEL = 0.01D0
      RM = -1.0D0/DEL
      RN = 1.0D0 + S/DEL
      DO 20 I = 1, NPTS
         PSI = X(I) - T
         IF (PSI.LT.S) THEN
            U(1,I) = 1.0D0
         ELSE IF (PSI.GT.(DEL+S)) THEN
            U(1,I) = 0.0D0
         ELSE
            U(1,I) = RM*PSI + RN
         END IF
   20 CONTINUE
      RETURN
      END
