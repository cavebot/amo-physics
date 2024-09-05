*     D02TXF Example Program Text
*     Mark 17 Release. NAG Copyright 1995.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          NEQ, MMAX, NLBC, NRBC, NCOL, MXMESH
      PARAMETER        (NEQ=2,MMAX=3,NLBC=3,NRBC=2,NCOL=6,MXMESH=250)
      INTEGER          LRWORK, LIWORK
      PARAMETER        (LRWORK=MXMESH*(109*NEQ**2+78*NEQ+7),
     +                 LIWORK=MXMESH*(11*NEQ+6))
*     .. Scalars in Common ..
      DOUBLE PRECISION EL, EN, S
*     .. Local Scalars ..
      DOUBLE PRECISION ERMX, XX
      INTEGER          I, IERMX, IFAIL, IJERMX, J, NCONT, NMESH
      LOGICAL          FAILED
*     .. Local Arrays ..
      DOUBLE PRECISION MESH(MXMESH), TOL(NEQ), WORK(LRWORK),
     +                 Y(NEQ,0:MMAX-1)
      INTEGER          IPMESH(MXMESH), IWORK(LIWORK), M(NEQ)
*     .. External Subroutines ..
      EXTERNAL         D02TKF, D02TVF, D02TXF, D02TYF, D02TZF, FFUN,
     +                 FJAC, GAFUN, GAJAC, GBFUN, GBJAC, GUESS
*     .. Intrinsic Functions ..
      INTRINSIC        DBLE
*     .. Common blocks ..
      COMMON           /PROBS/EN, S, EL
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D02TXF Example Program Results'
      WRITE (NOUT,*)
      NMESH = 21
      MESH(1) = 0.0D0
      IPMESH(1) = 1
      DO 20 I = 2, NMESH - 1
         MESH(I) = DBLE(I-1)/DBLE(NMESH-1)
         IPMESH(I) = 2
   20 CONTINUE
      IPMESH(NMESH) = 1
      MESH(NMESH) = 1.0D0
      M(1) = 3
      M(2) = 2
      TOL(1) = 1.0D-5
      TOL(2) = TOL(1)
      IFAIL = 0
      CALL D02TVF(NEQ,M,NLBC,NRBC,NCOL,TOL,MXMESH,NMESH,MESH,IPMESH,
     +            WORK,LRWORK,IWORK,LIWORK,IFAIL)
*     Initialize number of continuation steps
      NCONT = 3
*     Initialize problem dependent parameters
      EL = 6.0D1
      S = 0.24D0
      EN = 0.2D0
      DO 80 J = 1, NCONT
         WRITE (NOUT,99997) TOL(1), EL, S
         IFAIL = -1
*     Solve
         CALL D02TKF(FFUN,FJAC,GAFUN,GBFUN,GAJAC,GBJAC,GUESS,WORK,IWORK,
     +               IFAIL)
         FAILED = IFAIL .NE. 0
         IFAIL = 0
*     Extract mesh
         CALL D02TZF(MXMESH,NMESH,MESH,IPMESH,ERMX,IERMX,IJERMX,WORK,
     +               IWORK,IFAIL)
         WRITE (NOUT,99996) NMESH, ERMX, IERMX, IJERMX
         IF (FAILED) GO TO 100
*     Print solution components on mesh
         WRITE (NOUT,99999)
         DO 40 I = 1, 16
            XX = DBLE(I-1)*2.0D0/EL
            CALL D02TYF(XX,Y,NEQ,MMAX,WORK,IWORK,IFAIL)
            WRITE (NOUT,99998) XX*EL, Y(1,0), Y(2,0)
   40    CONTINUE
         DO 60 I = 1, 10
            XX = (3.0D1+(EL-3.0D1)*DBLE(I)/10.0D0)/EL
            CALL D02TYF(XX,Y,NEQ,MMAX,WORK,IWORK,IFAIL)
            WRITE (NOUT,99998) XX*EL, Y(1,0), Y(2,0)
   60    CONTINUE
*     Select mesh for continuation
         IF (J.LT.NCONT) THEN
            EL = 2.0D0*EL
            S = 0.6D0*S
            NMESH = (NMESH+1)/2
            CALL D02TXF(MXMESH,NMESH,MESH,IPMESH,WORK,IWORK,IFAIL)
         END IF
   80 CONTINUE
  100 CONTINUE
      STOP
*
99999 FORMAT (/' Solution on original interval:',/'    ','  x        f',
     +       '          g')
99998 FORMAT (' ',F8.2,2F11.4)
99997 FORMAT (//' Tolerance = ',D8.1,'  L = ',F8.3,'  S = ',F6.4)
99996 FORMAT (/' Used a mesh of ',I4,' points',/' Maximum error = ',
     +       D10.2,'  in interval ',I4,' for component ',I4)
      END
      SUBROUTINE FFUN(X,Y,NEQ,M,F)
*     .. Scalar Arguments ..
      DOUBLE PRECISION X
      INTEGER         NEQ
*     .. Array Arguments ..
      DOUBLE PRECISION F(NEQ), Y(NEQ,0:*)
      INTEGER         M(NEQ)
*     .. Scalars in Common ..
      DOUBLE PRECISION EL, EN, S
*     .. Common blocks ..
      COMMON          /PROBS/EN, S, EL
*     .. Executable Statements ..
      F(1) = EL**3*(1.0D0-Y(2,0)**2) + EL**2*S*Y(1,1) -
     +       EL*(0.5D0*(3.0D0-EN)*Y(1,0)*Y(1,2)+EN*Y(1,1)**2)
      F(2) = EL**2*S*(Y(2,0)-1.0D0) - EL*(0.5D0*(3.0D0-EN)*Y(1,0)*Y(2,1)
     +       +(EN-1.0D0)*Y(1,1)*Y(2,0))
      RETURN
      END
      SUBROUTINE FJAC(X,Y,NEQ,M,DF)
*     .. Scalar Arguments ..
      DOUBLE PRECISION X
      INTEGER         NEQ
*     .. Array Arguments ..
      DOUBLE PRECISION DF(NEQ,NEQ,0:*), Y(NEQ,0:*)
      INTEGER         M(NEQ)
*     .. Scalars in Common ..
      DOUBLE PRECISION EL, EN, S
*     .. Common blocks ..
      COMMON          /PROBS/EN, S, EL
*     .. Executable Statements ..
      DF(1,2,0) = -2.0D0*EL**3*Y(2,0)
      DF(1,1,0) = -EL*0.5D0*(3.0D0-EN)*Y(1,2)
      DF(1,1,1) = EL**2*S - EL*2.0D0*EN*Y(1,1)
      DF(1,1,2) = -EL*0.5D0*(3.0D0-EN)*Y(1,0)
      DF(2,2,0) = EL**2*S - EL*(EN-1.0D0)*Y(1,1)
      DF(2,2,1) = -EL*0.5D0*(3.0D0-EN)*Y(1,0)
      DF(2,1,0) = -EL*0.5D0*(3.0D0-EN)*Y(2,1)
      DF(2,1,1) = -EL*(EN-1.0D0)*Y(2,0)
      RETURN
      END
      SUBROUTINE GAFUN(YA,NEQ,M,NLBC,GA)
*     .. Scalar Arguments ..
      INTEGER          NEQ, NLBC
*     .. Array Arguments ..
      DOUBLE PRECISION GA(NLBC), YA(NEQ,0:*)
      INTEGER          M(NEQ)
*     .. Executable Statements ..
      GA(1) = YA(1,0)
      GA(2) = YA(1,1)
      GA(3) = YA(2,0)
      RETURN
      END
      SUBROUTINE GBFUN(YB,NEQ,M,NRBC,GB)
*     .. Scalar Arguments ..
      INTEGER          NEQ, NRBC
*     .. Array Arguments ..
      DOUBLE PRECISION GB(NRBC), YB(NEQ,0:*)
      INTEGER          M(NEQ)
*     .. Executable Statements ..
      GB(1) = YB(1,1)
      GB(2) = YB(2,0) - 1.0D0
      RETURN
      END
      SUBROUTINE GAJAC(YA,NEQ,M,NLBC,DGA)
*     .. Scalar Arguments ..
      INTEGER          NEQ, NLBC
*     .. Array Arguments ..
      DOUBLE PRECISION DGA(NLBC,NEQ,0:*), YA(NEQ,0:*)
      INTEGER          M(NEQ)
*     .. Executable Statements ..
      DGA(1,1,0) = 1.0D0
      DGA(2,1,1) = 1.0D0
      DGA(3,2,0) = 1.0D0
      RETURN
      END
      SUBROUTINE GBJAC(YB,NEQ,M,NRBC,DGB)
*     .. Scalar Arguments ..
      INTEGER          NEQ, NRBC
*     .. Array Arguments ..
      DOUBLE PRECISION DGB(NRBC,NEQ,0:*), YB(NEQ,0:*)
      INTEGER          M(NEQ)
*     .. Executable Statements ..
      DGB(1,1,1) = 1.0D0
      DGB(2,2,0) = 1.0D0
      RETURN
      END
      SUBROUTINE GUESS(X,NEQ,M,Z,DMVAL)
*     .. Scalar Arguments ..
      DOUBLE PRECISION X
      INTEGER          NEQ
*     .. Array Arguments ..
      DOUBLE PRECISION DMVAL(NEQ), Z(NEQ,0:*)
      INTEGER          M(NEQ)
*     .. Scalars in Common ..
      DOUBLE PRECISION EL, EN, S
*     .. Local Scalars ..
      DOUBLE PRECISION EX, EXPMX
*     .. Intrinsic Functions ..
      INTRINSIC        EXP
*     .. Common blocks ..
      COMMON           /PROBS/EN, S, EL
*     .. Executable Statements ..
      EX = X*EL
      EXPMX = EXP(-EX)
      Z(1,0) = -EX**2*EXPMX
      Z(1,1) = (-2.0D0*EX+EX**2)*EXPMX
      Z(1,2) = (-2.0D0+4.0D0*EX-EX**2)*EXPMX
      Z(2,0) = 1.0D0 - EXPMX
      Z(2,1) = EXPMX
      DMVAL(1) = (6.0D0-6.0D0*EX+EX**2)*EXPMX
      DMVAL(2) = -EXPMX
      RETURN
      END
