*     D02MVF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          NEQ, NEQMAX, NRW, NINF, NWKJAC, MAXORD, NY2DIM,
     +                 MAXSTP, MXHNIL
      PARAMETER        (NEQ=6,NEQMAX=NEQ,NRW=50+4*NEQMAX,NINF=23,
     +                 NWKJAC=NEQMAX*(NEQMAX+1),MAXORD=5,
     +                 NY2DIM=MAXORD+3,MAXSTP=5000,MXHNIL=5)
      DOUBLE PRECISION H0, HMAX, HMIN
      PARAMETER        (H0=0.0D0,HMAX=0.0D0,HMIN=1.0D-10)
*     .. Local Scalars ..
      DOUBLE PRECISION DUM, PI, T, TCRIT, TOUT
      INTEGER          I, IFAIL, ITASK, ITOL, ITRACE, MAXOD1
*     .. Local Arrays ..
      DOUBLE PRECISION ATOL(NEQMAX), CONST(3), RTOL(NEQMAX), RWORK(NRW),
     +                 WKJAC(NWKJAC), Y(NEQMAX), YDOT(NEQMAX),
     +                 YSAVE(NEQMAX,NY2DIM)
      INTEGER          INFORM(NINF)
      LOGICAL          LDERIV(2)
*     .. External Functions ..
      DOUBLE PRECISION X01AAF
      EXTERNAL         X01AAF
*     .. External Subroutines ..
      EXTERNAL         D02MVF, D02NBY, D02NGF, D02NSF, DAEJAC, DAERES,
     +                 X04ABF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D02MVF Example Program Results'
      CALL X04ABF(1,NOUT)
      DO 20 I = 1, 3
         CONST(I) = 0.0D0
   20 CONTINUE
      ITRACE = 0
      PI = X01AAF(DUM)
      RTOL(1) = 1.0D-3
      ATOL(1) = 1.0D-6
      WRITE (NOUT,*)
      WRITE (NOUT,99999) 'Pendulum problem with relative tolerance = ',
     +  RTOL(1)
      WRITE (NOUT,99999) '                  and absolute tolerance = ',
     +  ATOL(1)
      ITOL = 1
      T = 0.0D0
      TOUT = PI
*     Set initial values and derivatives
      Y(1) = 1.D0
      Y(2) = 0.0D0
      Y(3) = 0.D0
      Y(4) = 0.D0
      Y(5) = 0.0D0
      Y(6) = 0.0D0
      YDOT(1) = Y(3) - Y(6)*Y(1)
      YDOT(2) = Y(4) - Y(6)*Y(2)
      YDOT(3) = -Y(5)*Y(1)
      YDOT(4) = -Y(5)*Y(2) - 1.D0
      YDOT(5) = -3.D0*Y(4)
      YDOT(6) = 0.0D0
      WRITE (NOUT,*)
      WRITE (NOUT,*)
     +  '  t          y1      y2      y3      y4      y5      y6'
      WRITE (NOUT,99998) T, (Y(I),I=1,6)
      ITASK = 4
      TCRIT = TOUT
      IFAIL = 0
      MAXOD1 = 0
*
      CALL D02MVF(NEQMAX,NY2DIM,MAXOD1,CONST,TCRIT,HMIN,HMAX,H0,MAXSTP,
     +            MXHNIL,'AVERAGE-L2',RWORK,IFAIL)
*
      CALL D02NSF(NEQ,NEQMAX,'ANALYTIC',NWKJAC,RWORK,IFAIL)
*
      IFAIL = -1
      LDERIV(1) = .TRUE.
      LDERIV(2) = .TRUE.
*
      CALL D02NGF(NEQ,NEQMAX,T,TOUT,Y,YDOT,RWORK,RTOL,ATOL,ITOL,INFORM,
     +            DAERES,YSAVE,NY2DIM,DAEJAC,WKJAC,NWKJAC,D02NBY,LDERIV,
     +            ITASK,ITRACE,IFAIL)
*
      IF (IFAIL.NE.0) THEN
         WRITE (NOUT,99997) 'Integration terminated with IFAIL = ',
     +     IFAIL, ' at T = ', T
      ELSE
         WRITE (NOUT,99998) T, (Y(I),I=1,NEQ)
      END IF
      STOP
*
99999 FORMAT (1X,A,1P,D7.1)
99998 FORMAT (1X,F6.4,3X,6(F8.4))
99997 FORMAT (1X,A,I2,A,F6.4)
      END
      SUBROUTINE DAERES(NEQ,T,Y,YDOT,R,IRES)
*     .. Scalar Arguments ..
      DOUBLE PRECISION  T
      INTEGER           IRES, NEQ
*     .. Array Arguments ..
      DOUBLE PRECISION  R(NEQ), Y(NEQ), YDOT(NEQ)
*     .. Executable Statements ..
      IF (IRES.EQ.-1) THEN
         R(1) = -YDOT(1)
         R(2) = -YDOT(2)
         R(3) = -YDOT(3)
         R(4) = -YDOT(4)
         R(5) = 0.0D0
         R(6) = 0.0D0
      ELSE
         R(1) = Y(3) - Y(6)*Y(1) - YDOT(1)
         R(2) = Y(4) - Y(6)*Y(2) - YDOT(2)
         R(3) = -Y(5)*Y(1) - YDOT(3)
         R(4) = -Y(5)*Y(2) - 1.D0 - YDOT(4)
         R(5) = Y(1)*Y(3) + Y(2)*Y(4)
         R(6) = Y(1)**2 + Y(2)**2 - 1.0D0
      END IF
      RETURN
      END
      SUBROUTINE DAEJAC(NEQ,T,Y,YDOT,H,D,P)
*     .. Parameters ..
      DOUBLE PRECISION  ONE, TWO
      PARAMETER         (ONE=1.0D0,TWO=2.0D0)
*     .. Scalar Arguments ..
      DOUBLE PRECISION  D, H, T
      INTEGER           NEQ
*     .. Array Arguments ..
      DOUBLE PRECISION  P(NEQ,NEQ), Y(NEQ), YDOT(NEQ)
*     .. Local Scalars ..
      DOUBLE PRECISION  HXD
*     .. Executable Statements ..
      HXD = H*D
      P(1,1) = (ONE+HXD*Y(6))
      P(1,3) = -HXD
      P(1,6) = HXD*Y(1)
      P(2,2) = (ONE+HXD*Y(6))
      P(2,4) = -HXD
      P(2,6) = HXD*Y(2)
      P(3,1) = HXD*Y(5)
      P(3,3) = ONE
      P(3,5) = HXD*Y(1)
      P(4,2) = HXD*Y(5)
      P(4,4) = ONE
      P(4,5) = HXD*Y(2)
      P(5,1) = -HXD*Y(3)
      P(5,2) = -HXD*Y(4)
      P(5,3) = -HXD*Y(1)
      P(5,4) = -HXD*Y(2)
      P(6,1) = -TWO*HXD*Y(1)
      P(6,2) = -TWO*HXD*Y(2)
      RETURN
      END
