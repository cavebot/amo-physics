*     D03PFF Example Program Text
*     Mark 17 Release. NAG Copyright 1995.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. External Subroutines ..
      EXTERNAL         EX1, EX2
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D03PFF Example Program Results'
      CALL EX1
      CALL EX2
      STOP
      END
*
      SUBROUTINE EX1
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          NPDE, NPTS, NIW, NW, INT, OUTPTS
      PARAMETER        (NPDE=2,NPTS=101,NIW=24+NPDE*NPTS,NW=(11+9*NPDE)
     +                 *NPDE*NPTS+(32+3*NPDE)*NPDE+7*NPTS+54,INT=20,
     +                 OUTPTS=7)
*     .. Scalars in Common ..
      DOUBLE PRECISION P
*     .. Local Scalars ..
      DOUBLE PRECISION TOUT, TS, TSMAX, XX
      INTEGER          I, IFAIL, IND, IT, ITASK, ITRACE, J, NOP
*     .. Local Arrays ..
      DOUBLE PRECISION ACC(2), U(NPDE,NPTS), UE(NPDE,OUTPTS), W(NW),
     +                 X(NPTS), XOUT(OUTPTS)
      INTEGER          IW(NIW)
*     .. External Functions ..
      DOUBLE PRECISION X01AAF
      EXTERNAL         X01AAF
*     .. External Subroutines ..
      EXTERNAL         BNDRY1, D03PFF, D03PFP, EXACT, NMFLX1
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
      ACC(1) = 0.1D-3
      ACC(2) = 0.1D-4
      TSMAX = 0.0D0
      WRITE (NOUT,99996) NPTS, ACC(1), ACC(2)
      WRITE (NOUT,99999)
*
*     Initialise mesh ..
*
      DO 20 I = 1, NPTS
         X(I) = (I-1.0D0)/(NPTS-1.0D0)
   20 CONTINUE
*
*     Set initial values ..
      TS = 0.0D0
      CALL EXACT(TS,U,NPDE,X,NPTS)
*
      IND = 0
      ITASK = 1
*
      DO 80 IT = 1, 2
         TOUT = 0.1D0*IT
         IFAIL = 0
*
         CALL D03PFF(NPDE,TS,TOUT,D03PFP,NMFLX1,BNDRY1,U,NPTS,X,ACC,
     +               TSMAX,W,NW,IW,NIW,ITASK,ITRACE,IND,IFAIL)
*
*       Set output points ..
         NOP = 0
         DO 40 I = 1, NPTS, INT
            NOP = NOP + 1
            XOUT(NOP) = X(I)
   40    CONTINUE
*
         WRITE (NOUT,99995) TS
*
*       Check against exact solution ..
         CALL EXACT(TOUT,UE,NPDE,XOUT,NOP)
         DO 60 I = 1, NOP
            J = 1 + INT*(I-1)
            WRITE (NOUT,99998) XOUT(I), U(1,J), UE(1,I), U(2,J),
     +        UE(2,I)
   60    CONTINUE
   80 CONTINUE
*
      WRITE (NOUT,99997) IW(1), IW(2), IW(3), IW(5)
      RETURN
*
99999 FORMAT (8X,'X',8X,'Approx U',4X,'Exact U',5X,'Approx V',4X,'Exac',
     +       't V')
99998 FORMAT (5(3X,F9.4))
99997 FORMAT (/' Number of integration steps in time = ',I6,/' Number ',
     +       'of function evaluations = ',I6,/' Number of Jacobian ',
     +       'evaluations =',I6,/' Number of iterations = ',I6,/)
99996 FORMAT (/' NPTS = ',I4,' ACC(1) = ',D10.3,' ACC(2) = ',D10.3,/)
99995 FORMAT (/' T = ',F6.3,/)
      END
*
      SUBROUTINE BNDRY1(NPDE,NPTS,T,X,U,IBND,G,IRES)
*     .. Scalar Arguments ..
      DOUBLE PRECISION  T
      INTEGER           IBND, IRES, NPDE, NPTS
*     .. Array Arguments ..
      DOUBLE PRECISION  G(NPDE), U(NPDE,3), X(NPTS)
*     .. Local Scalars ..
      DOUBLE PRECISION  C, EXU1, EXU2
*     .. Local Arrays ..
      DOUBLE PRECISION  UE(2,1)
*     .. External Subroutines ..
      EXTERNAL          EXACT
*     .. Executable Statements ..
      IF (IBND.EQ.0) THEN
         CALL EXACT(T,UE,NPDE,X(1),1)
         C = (X(2)-X(1))/(X(3)-X(2))
         EXU1 = (1.0D0+C)*U(1,2) - C*U(1,3)
         EXU2 = (1.0D0+C)*U(2,2) - C*U(2,3)
         G(1) = 2.0D0*U(1,1) + U(2,1) - 2.0D0*UE(1,1) - UE(2,1)
         G(2) = 2.0D0*U(1,1) - U(2,1) - 2.0D0*EXU1 + EXU2
      ELSE
         CALL EXACT(T,UE,NPDE,X(NPTS),1)
         C = (X(NPTS)-X(NPTS-1))/(X(NPTS-1)-X(NPTS-2))
         EXU1 = (1.0D0+C)*U(1,2) - C*U(1,3)
         EXU2 = (1.0D0+C)*U(2,2) - C*U(2,3)
         G(1) = 2.0D0*U(1,1) - U(2,1) - 2.0D0*UE(1,1) + UE(2,1)
         G(2) = 2.0D0*U(1,1) + U(2,1) - 2.0D0*EXU1 - EXU2
      END IF
      RETURN
      END
*
      SUBROUTINE NMFLX1(NPDE,T,X,ULEFT,URIGHT,FLUX,IRES)
*     .. Scalar Arguments ..
      DOUBLE PRECISION  T, X
      INTEGER           IRES, NPDE
*     .. Array Arguments ..
      DOUBLE PRECISION  FLUX(NPDE), ULEFT(NPDE), URIGHT(NPDE)
*     .. Executable Statements ..
      FLUX(1) = 0.5D0*(-URIGHT(1)+3.0D0*ULEFT(1)+0.5D0*URIGHT(2)
     +          +1.5D0*ULEFT(2))
      FLUX(2) = 0.5D0*(2.0D0*URIGHT(1)+6.0D0*ULEFT(1)-URIGHT(2)
     +          +3.0D0*ULEFT(2))
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
      DOUBLE PRECISION X1, X2
      INTEGER          I
*     .. Intrinsic Functions ..
      INTRINSIC        EXP, SIN
*     .. Common blocks ..
      COMMON           /PI/P
*     .. Executable Statements ..
      DO 20 I = 1, NPTS
         X1 = X(I) + T
         X2 = X(I) - 3.0D0*T
         U(1,I) = 0.5D0*(EXP(X1)+EXP(X2)) + 0.25D0*(SIN(2.0D0*P*X2**2)
     +            -SIN(2.0D0*P*X1**2)) + 2.0D0*T**2 - 2.0D0*X(I)*T
         U(2,I) = EXP(X2) - EXP(X1) + 0.5D0*(SIN(2.0D0*P*X2**2)
     +            +SIN(2.0D0*P*X1**2)) + X(I)**2 + 5.0D0*T**2 -
     +            2.0D0*X(I)*T
   20 CONTINUE
      RETURN
      END
*
      SUBROUTINE EX2
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          NPDE, NPTS, NIW, NW, OUTPTS
      PARAMETER        (NPDE=1,NPTS=151,NIW=24+NPDE*NPTS,NW=(11+9*NPDE)
     +                 *NPDE*NPTS+(32+3*NPDE)*NPDE+7*NPTS+54,OUTPTS=7)
*     .. Local Scalars ..
      DOUBLE PRECISION TOUT, TS, TSMAX
      INTEGER          I, IFAIL, IND, IT, ITASK, ITRACE
*     .. Local Arrays ..
      DOUBLE PRECISION ACC(2), U(NPDE,NPTS), W(NW), X(NPTS),
     +                 XOUT(OUTPTS)
      INTEGER          IW(NIW)
*     .. External Subroutines ..
      EXTERNAL         BNDRY2, D03PFF, NMFLX2, PDEDEF
*     .. Executable Statements ..
      WRITE (NOUT,*)
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Example 2'
      WRITE (NOUT,*)
*
      ITRACE = 0
      ACC(1) = 0.1D-4
      ACC(2) = 0.1D-4
      WRITE (NOUT,99998) NPTS, ACC(1), ACC(2)
*
*     Initialise mesh ..
*
      DO 20 I = 1, NPTS
         X(I) = -1.0D0 + 2.0D0*(I-1.0D0)/(NPTS-1.0D0)
   20 CONTINUE
*
*     Set initial values ..
      DO 40 I = 1, NPTS
         U(1,I) = X(I) + 4.0D0
   40 CONTINUE
*
      IND = 0
      ITASK = 1
      TSMAX = 0.2D-1
*
*     Set output points ..
      XOUT(1) = X(1)
      XOUT(2) = X(4)
      XOUT(3) = X(37)
      XOUT(4) = X(76)
      XOUT(5) = X(112)
      XOUT(6) = X(148)
      XOUT(7) = X(151)
*
      WRITE (NOUT,99996) (XOUT(I),I=1,OUTPTS)
*
*     Loop over output value of t
*
      TS = 0.0D0
      TOUT = 1.0D0
      DO 60 IT = 1, 2
         IF (IT.EQ.2) TOUT = 10.0D0
         IFAIL = 0
*
         CALL D03PFF(NPDE,TS,TOUT,PDEDEF,NMFLX2,BNDRY2,U,NPTS,X,ACC,
     +               TSMAX,W,NW,IW,NIW,ITASK,ITRACE,IND,IFAIL)
*
         WRITE (NOUT,99999) TS
         WRITE (NOUT,99995) U(1,1), U(1,4), U(1,37), U(1,76),
     +     U(1,112), U(1,148), U(1,151)
   60 CONTINUE
*
      WRITE (NOUT,99997) IW(1), IW(2), IW(3), IW(5)
      RETURN
*
99999 FORMAT (' T = ',F6.3)
99998 FORMAT (/' NPTS = ',I4,'  ACC(1) = ',D10.3,' ACC(2) = ',D10.3,/)
99997 FORMAT (' Number of integration steps in time = ',I6,/' Number ',
     +       'of function evaluations = ',I6,/' Number of Jacobian ',
     +       'evaluations =',I6,/' Number of iterations = ',I6,/)
99996 FORMAT (1X,'X    ',7F9.4,/)
99995 FORMAT (1X,'U    ',7F9.4,/)
      END
*
      SUBROUTINE PDEDEF(NPDE,T,X,U,UX,P,C,D,S,IRES)
*     .. Scalar Arguments ..
      DOUBLE PRECISION  T, X
      INTEGER           IRES, NPDE
*     .. Array Arguments ..
      DOUBLE PRECISION  C(NPDE), D(NPDE), P(NPDE,NPDE), S(NPDE),
     +                  U(NPDE), UX(NPDE)
*     .. Executable Statements ..
      P(1,1) = 1.0D0
      C(1) = 0.1D-1
      D(1) = UX(1)
      S(1) = U(1)
      RETURN
      END
*
      SUBROUTINE BNDRY2(NPDE,NPTS,T,X,U,IBND,G,IRES)
*     .. Scalar Arguments ..
      DOUBLE PRECISION  T
      INTEGER           IBND, IRES, NPDE, NPTS
*     .. Array Arguments ..
      DOUBLE PRECISION  G(NPDE), U(NPDE,3), X(NPTS)
*     .. Executable Statements ..
      IF (IBND.EQ.0) THEN
         G(1) = U(1,1) - 3.0D0
      ELSE
         G(1) = U(1,1) - 5.0D0
      END IF
      RETURN
      END
*
      SUBROUTINE NMFLX2(NPDE,T,X,ULEFT,URIGHT,FLUX,IRES)
*     .. Scalar Arguments ..
      DOUBLE PRECISION  T, X
      INTEGER           IRES, NPDE
*     .. Array Arguments ..
      DOUBLE PRECISION  FLUX(NPDE), ULEFT(NPDE), URIGHT(NPDE)
*     .. Executable Statements ..
      IF (X.GE.0) THEN
         FLUX(1) = X*ULEFT(1)
      ELSE
         FLUX(1) = X*URIGHT(1)
      END IF
      RETURN
      END
