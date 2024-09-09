*     D02TZF Example Program Text
*     Mark 17 Release. NAG Copyright 1995.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          NEQ, MMAX, NLBC, NRBC, NCOL, MXMESH
      PARAMETER        (NEQ=1,MMAX=2,NLBC=1,NRBC=1,NCOL=5,MXMESH=50)
      INTEGER          LRWORK, LIWORK
      PARAMETER        (LRWORK=MXMESH*(109*NEQ**2+78*NEQ+7),
     +                 LIWORK=MXMESH*(11*NEQ+6))
*     .. Scalars in Common ..
      DOUBLE PRECISION ALPHA, BETA, EPS
*     .. Local Scalars ..
      DOUBLE PRECISION ERMX
      INTEGER          I, IERMX, IFAIL, IJERMX, J, NMESH
      LOGICAL          FAILED
*     .. Local Arrays ..
      DOUBLE PRECISION MESH(MXMESH), TOL(NEQ), WORK(LRWORK),
     +                 Y(NEQ,0:MMAX-1)
      INTEGER          IPMESH(MXMESH), IWORK(LIWORK), M(NEQ)
*     .. External Subroutines ..
      EXTERNAL         D02TKF, D02TVF, D02TXF, D02TYF, D02TZF, FFUN,
     +                 FJAC, GAFUN, GAJAC, GBFUN, GBJAC, GUESS
*     .. Common blocks ..
      COMMON           /PROBS/EPS, ALPHA, BETA
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D02TZF Example Program Results'
      WRITE (NOUT,*)
      NMESH = 7
      MESH(1) = 0.0D0
      MESH(2) = 0.15D0
      MESH(3) = 0.3D0
      MESH(4) = 0.5D0
      MESH(5) = 0.7D0
      MESH(6) = 0.85D0
      MESH(NMESH) = 1.0D0
      IPMESH(1) = 1
      IPMESH(2) = 2
      IPMESH(3) = 1
      IPMESH(4) = 2
      IPMESH(5) = 1
      IPMESH(6) = 2
      IPMESH(NMESH) = 1
      ALPHA = -1.0D0/3.0D0
      BETA = 1.0D0/3.0D0
      TOL(1) = 1.0D-5
      EPS = 1.0D-3
      M(1) = 2
      IFAIL = 0
      CALL D02TVF(NEQ,M,NLBC,NRBC,NCOL,TOL,MXMESH,NMESH,MESH,IPMESH,
     +            WORK,LRWORK,IWORK,LIWORK,IFAIL)
      IFAIL = -1
      DO 40 J = 1, 2
         EPS = 0.1D0*EPS
         WRITE (NOUT,99997) TOL(1), EPS
         IFAIL = -1
         CALL D02TKF(FFUN,FJAC,GAFUN,GBFUN,GAJAC,GBJAC,GUESS,WORK,IWORK,
     +               IFAIL)
         FAILED = IFAIL .NE. 0
         IFAIL = 0
         CALL D02TZF(MXMESH,NMESH,MESH,IPMESH,ERMX,IERMX,IJERMX,WORK,
     +               IWORK,IFAIL)
         WRITE (NOUT,99996) NMESH, ERMX, IERMX, IJERMX
         IF (FAILED) GO TO 60
         WRITE (NOUT,99999)
         DO 20 I = 1, NMESH, 2
            CALL D02TYF(MESH(I),Y,NEQ,MMAX,WORK,IWORK,IFAIL)
            WRITE (NOUT,99998) MESH(I), Y(1,0), Y(1,1)
   20    CONTINUE
         IF (J.LT.2) THEN
            NMESH = (NMESH+1)/2
            CALL D02TXF(MXMESH,NMESH,MESH,IPMESH,WORK,IWORK,IFAIL)
         END IF
   40 CONTINUE
   60 CONTINUE
      STOP
*
99999 FORMAT (/' Solution and derivative at every second point:',
     +       /'    ','  x        u          u''')
99998 FORMAT (' ',F8.3,2F11.5)
99997 FORMAT (//' Tolerance = ',D8.1,'  EPS = ',D10.3)
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
      DOUBLE PRECISION ALPHA, BETA, EPS
*     .. Common blocks ..
      COMMON          /PROBS/EPS, ALPHA, BETA
*     .. Executable Statements ..
      F(1) = (Y(1,0)-Y(1,0)*Y(1,1))/EPS
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
      DOUBLE PRECISION ALPHA, BETA, EPS
*     .. Local Scalars ..
      DOUBLE PRECISION FAC, MACHEP, PTRB
      INTEGER         I, J, K
*     .. Local Arrays ..
      DOUBLE PRECISION F1(1), F2(1), YP(1,0:3)
*     .. External Functions ..
      DOUBLE PRECISION X02AJF
      EXTERNAL        X02AJF
*     .. External Subroutines ..
      EXTERNAL        FFUN
*     .. Intrinsic Functions ..
      INTRINSIC       ABS, MAX, SQRT
*     .. Common blocks ..
      COMMON          /PROBS/EPS, ALPHA, BETA
*     .. Executable Statements ..
      MACHEP = X02AJF()
      FAC = SQRT(MACHEP)
      DO 40 I = 1, NEQ
         DO 20 J = 0, M(I) - 1
            YP(I,J) = Y(I,J)
   20    CONTINUE
   40 CONTINUE
      DO 100 I = 1, NEQ
         DO 80 J = 0, M(I) - 1
            PTRB = MAX(1.0D2*MACHEP,FAC*ABS(Y(I,J)))
            YP(I,J) = Y(I,J) + PTRB
            CALL FFUN(X,YP,NEQ,M,F1)
            YP(I,J) = Y(I,J) - PTRB
            CALL FFUN(X,YP,NEQ,M,F2)
            DO 60 K = 1, NEQ
               DF(K,I,J) = 0.5D0*(F1(K)-F2(K))/PTRB
   60       CONTINUE
            YP(I,J) = Y(I,J)
   80    CONTINUE
  100 CONTINUE
      RETURN
      END
      SUBROUTINE GAFUN(YA,NEQ,M,NLBC,GA)
*     .. Scalar Arguments ..
      INTEGER          NEQ, NLBC
*     .. Array Arguments ..
      DOUBLE PRECISION GA(NLBC), YA(NEQ,0:*)
      INTEGER          M(NEQ)
*     .. Scalars in Common ..
      DOUBLE PRECISION ALPHA, BETA, EPS
*     .. Common blocks ..
      COMMON           /PROBS/EPS, ALPHA, BETA
*     .. Executable Statements ..
      GA(1) = YA(1,0) - ALPHA
      RETURN
      END
      SUBROUTINE GBFUN(YB,NEQ,M,NRBC,GB)
*     .. Scalar Arguments ..
      INTEGER          NEQ, NRBC
*     .. Array Arguments ..
      DOUBLE PRECISION GB(NRBC), YB(NEQ,0:*)
      INTEGER          M(NEQ)
*     .. Scalars in Common ..
      DOUBLE PRECISION ALPHA, BETA, EPS
*     .. Common blocks ..
      COMMON           /PROBS/EPS, ALPHA, BETA
*     .. Executable Statements ..
      GB(1) = YB(1,0) - BETA
      RETURN
      END
      SUBROUTINE GAJAC(YA,NEQ,M,NLBC,DGA)
*     .. Parameters ..
      DOUBLE PRECISION ONE
      PARAMETER        (ONE=1.0D+0)
*     .. Scalar Arguments ..
      INTEGER          NEQ, NLBC
*     .. Array Arguments ..
      DOUBLE PRECISION DGA(NLBC,NEQ,0:*), YA(NEQ,0:*)
      INTEGER          M(NEQ)
*     .. Executable Statements ..
      DGA(1,1,0) = ONE
      RETURN
      END
      SUBROUTINE GBJAC(YB,NEQ,M,NRBC,DGB)
*     .. Parameters ..
      DOUBLE PRECISION ONE
      PARAMETER        (ONE=1.0D+0)
*     .. Scalar Arguments ..
      INTEGER          NEQ, NRBC
*     .. Array Arguments ..
      DOUBLE PRECISION DGB(NRBC,NEQ,0:*), YB(NEQ,0:*)
      INTEGER          M(NEQ)
*     .. Executable Statements ..
      DGB(1,1,0) = ONE
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
      DOUBLE PRECISION ALPHA, BETA, EPS
*     .. Common blocks ..
      COMMON           /PROBS/EPS, ALPHA, BETA
*     .. Executable Statements ..
      Z(1,0) = ALPHA + (BETA-ALPHA)*X
      Z(1,1) = (BETA-ALPHA)
      DMVAL(1) = 0.0D0
      RETURN
      END
