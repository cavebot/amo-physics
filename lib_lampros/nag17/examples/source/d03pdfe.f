*     D03PDF Example Program Text
*     Mark 15 Release. NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          NBKPTS, NEL, NPDE, NPOLY, M, NPTS, NEQN, NIW,
     +                 NPL1, NWKRES, MU, LENODE, NW, ITYPE, INTPTS
      PARAMETER        (NBKPTS=10,NEL=NBKPTS-1,NPDE=2,NPOLY=3,M=0,
     +                 NPTS=NEL*NPOLY+1,NEQN=NPDE*NPTS,NIW=NEQN+24,
     +                 NPL1=NPOLY+1,NWKRES=3*NPL1*NPL1+NPL1*
     +                 (NPDE*NPDE+6*NPDE+NBKPTS+1)+13*NPDE+5,
     +                 MU=NPDE*(NPOLY+1)-1,LENODE=(3*MU+1)*NEQN,
     +                 NW=11*NEQN+50+NWKRES+LENODE,ITYPE=1,INTPTS=6)
*     .. Scalars in Common ..
      DOUBLE PRECISION PIBY2
*     .. Local Scalars ..
      DOUBLE PRECISION ACC, PI, TOUT, TS
      INTEGER          I, IFAIL, IND, IT, ITASK, ITRACE
*     .. Local Arrays ..
      DOUBLE PRECISION U(NPDE,NPTS), UOUT(NPDE,INTPTS,ITYPE), W(NW),
     +                 X(NPTS), XBKPTS(NBKPTS), XOUT(6)
      INTEGER          IW(NIW)
*     .. External Functions ..
      DOUBLE PRECISION X01AAF
      EXTERNAL         X01AAF
*     .. External Subroutines ..
      EXTERNAL         BNDARY, D03PDF, D03PYF, PDEDEF, UINIT
*     .. Common blocks ..
      COMMON           /PIVAL/PIBY2
*     .. Data statements ..
      DATA             XOUT(1)/-1.0D+0/, XOUT(2)/-0.6D+0/,
     +                 XOUT(3)/-0.2D+0/, XOUT(4)/0.2D+0/,
     +                 XOUT(5)/0.6D+0/, XOUT(6)/1.0D+0/
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D03PDF Example Program Results'
      PIBY2 = 0.5D0*X01AAF(PI)
      ACC = 1.0D-4
      ITRACE = 0
*
*     Set the break-points
*
      DO 20 I = 1, NBKPTS
         XBKPTS(I) = -1.0D0 + (I-1.0D0)*2.0D0/(NBKPTS-1.0D0)
   20 CONTINUE
*
      IND = 0
      ITASK = 1
      TS = 0.0D0
      TOUT = 0.1D-4
      WRITE (NOUT,99999) NPOLY, NEL
      WRITE (NOUT,99998) ACC, NPTS
      WRITE (NOUT,99997) (XOUT(I),I=1,6)
*
*     Loop over output values of t
*
      DO 40 IT = 1, 5
         IFAIL = -1
         TOUT = 10.0D0*TOUT
*
         CALL D03PDF(NPDE,M,TS,TOUT,PDEDEF,BNDARY,U,NBKPTS,XBKPTS,NPOLY,
     +               NPTS,X,UINIT,ACC,W,NW,IW,NIW,ITASK,ITRACE,IND,
     +               IFAIL)
*
*        Interpolate at required spatial points
*
         CALL D03PYF(NPDE,U,NBKPTS,XBKPTS,NPOLY,NPTS,XOUT,INTPTS,ITYPE,
     +               UOUT,W,NW,IFAIL)
         WRITE (NOUT,99996) TS, (UOUT(1,I,1),I=1,INTPTS)
         WRITE (NOUT,99995) (UOUT(2,I,1),I=1,INTPTS)
   40 CONTINUE
*
*     Print integration statistics
*
      WRITE (NOUT,99994) IW(1), IW(2), IW(3), IW(5)
      STOP
*
99999 FORMAT (' Polynomial degree =',I4,'   No. of elements = ',I4)
99998 FORMAT (' Accuracy requirement = ',D9.3,'  Number of points = ',
     +       I5,/)
99997 FORMAT ('  T /   X   ',6F8.4,/)
99996 FORMAT (1X,F6.4,' U(1)',6F8.4)
99995 FORMAT (8X,'U(2)',6F8.4,/)
99994 FORMAT (' Number of integration steps in time                  ',
     +       I4,/' Number of residual evaluations of resulting ODE sys',
     +       'tem',I4,/' Number of Jacobian evaluations               ',
     +       '         ',I4,/' Number of iterations of nonlinear solve',
     +       'r              ',I4,/)
      END
*
      SUBROUTINE UINIT(NPDE,NPTS,X,U)
*     .. Scalar Arguments ..
      INTEGER          NPDE, NPTS
*     .. Array Arguments ..
      DOUBLE PRECISION U(NPDE,NPTS), X(NPTS)
*     .. Scalars in Common ..
      DOUBLE PRECISION PIBY2
*     .. Local Scalars ..
      INTEGER          I
*     .. Intrinsic Functions ..
      INTRINSIC        SIN
*     .. Common blocks ..
      COMMON           /PIVAL/PIBY2
*     .. Executable Statements ..
      DO 20 I = 1, NPTS
         U(1,I) = -SIN(PIBY2*X(I))
         U(2,I) = -PIBY2*PIBY2*U(1,I)
   20 CONTINUE
      RETURN
      END
*
      SUBROUTINE PDEDEF(NPDE,T,X,NPTL,U,DUDX,P,Q,R,IRES)
*     .. Scalar Arguments ..
      DOUBLE PRECISION  T
      INTEGER           IRES, NPDE, NPTL
*     .. Array Arguments ..
      DOUBLE PRECISION  DUDX(NPDE,NPTL), P(NPDE,NPDE,NPTL),
     +                  Q(NPDE,NPTL), R(NPDE,NPTL), U(NPDE,NPTL),
     +                  X(NPTL)
*     .. Local Scalars ..
      INTEGER           I
*     .. Executable Statements ..
      DO 20 I = 1, NPTL
         Q(1,I) = U(2,I)
         Q(2,I) = U(1,I)*DUDX(2,I) - DUDX(1,I)*U(2,I)
         R(1,I) = DUDX(1,I)
         R(2,I) = DUDX(2,I)
         P(1,1,I) = 0.0D0
         P(1,2,I) = 0.0D0
         P(2,1,I) = 0.0D0
         P(2,2,I) = 1.0D0
   20 CONTINUE
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
         BETA(1) = 1.0D0
         GAMMA(1) = 0.0D0
         BETA(2) = 0.0D0
         GAMMA(2) = U(1) - 1.0D0
      ELSE
         BETA(1) = 1.0D+0
         GAMMA(1) = 0.0D0
         BETA(2) = 0.0D0
         GAMMA(2) = U(1) + 1.0D0
      END IF
      RETURN
      END
