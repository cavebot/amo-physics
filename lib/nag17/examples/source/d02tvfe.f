*     D02TVF Example Program Text
*     Mark 17 Release. NAG Copyright 1995.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          NEQ, MMAX, NLBC, NRBC, NCOL, MXMESH
      PARAMETER        (NEQ=6,MMAX=1,NLBC=3,NRBC=3,NCOL=5,MXMESH=100)
      INTEGER          LRWORK, LIWORK
      PARAMETER        (LRWORK=MXMESH*(109*NEQ**2+78*NEQ+7),
     +                 LIWORK=MXMESH*(11*NEQ+6))
*     .. Scalars in Common ..
      DOUBLE PRECISION BETA0, ETA, LAMBDA, MU, PI
*     .. Local Scalars ..
      DOUBLE PRECISION ERMX
      INTEGER          I, IERMX, IFAIL, IJERMX, NMESH
*     .. Local Arrays ..
      DOUBLE PRECISION MESH(MXMESH), RWORK(LRWORK), TOL(NEQ),
     +                 Y(NEQ,0:MMAX-1)
      INTEGER          IPMESH(MXMESH), IWORK(LIWORK), M(NEQ)
*     .. External Subroutines ..
      EXTERNAL         D02TKF, D02TVF, D02TYF, D02TZF, FFUN, FJAC,
     +                 GAFUN, GAJAC, GBFUN, GBJAC, GUESS
*     .. Intrinsic Functions ..
      INTRINSIC        ATAN
*     .. Common blocks ..
      COMMON           /PROB/ETA, MU, LAMBDA, BETA0, PI
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D02TVF Example Program Results'
      WRITE (NOUT,*)
      NMESH = 11
      MESH(1) = 0.0D0
      IPMESH(1) = 1
      DO 20 I = 2, NMESH - 1
         MESH(I) = 0.1D0*(I-1)
         IPMESH(I) = 2
   20 CONTINUE
      IPMESH(NMESH) = 1
      MESH(NMESH) = 1.0D0
      DO 40 I = 1, NEQ
         TOL(I) = 1.0D-5
         M(I) = 1
   40 CONTINUE
      ETA = 0.01D0
      MU = 0.02D0
      LAMBDA = 0.0279D0
      BETA0 = 1575.0D0
      PI = 4.0D0*ATAN(1.0D0)
      IFAIL = 0
      CALL D02TVF(NEQ,M,NLBC,NRBC,NCOL,TOL,MXMESH,NMESH,MESH,IPMESH,
     +            RWORK,LRWORK,IWORK,LIWORK,IFAIL)
      IFAIL = -1
      CALL D02TKF(FFUN,FJAC,GAFUN,GBFUN,GAJAC,GBJAC,GUESS,RWORK,IWORK,
     +            IFAIL)
      CALL D02TZF(MXMESH,NMESH,MESH,IPMESH,ERMX,IERMX,IJERMX,RWORK,
     +            IWORK,IFAIL)
      WRITE (NOUT,99999) NMESH, ERMX, IERMX, IJERMX,
     +  (I,IPMESH(I),MESH(I),I=1,NMESH)
      WRITE (NOUT,99998)
      DO 60 I = 1, NMESH
         IFAIL = 1
         CALL D02TYF(MESH(I),Y,NEQ,MMAX,RWORK,IWORK,IFAIL)
         WRITE (NOUT,99997) MESH(I), Y(1,0), Y(2,0), Y(3,0)
   60 CONTINUE
      STOP
*
99999 FORMAT (/' Used a mesh of ',I4,' points',/' Maximum error = ',
     +       D10.2,'  in interval ',I4,' for component ',I4,//' Mesh p',
     +       'oints:',/4(I4,'(',I1,')',F7.4))
99998 FORMAT (/' Computed solution at mesh points',/'    x       y1   ',
     +       '      y2         y3')
99997 FORMAT (' ',F6.3,1X,3D11.3)
      END
      SUBROUTINE FFUN(X,Y,NEQ,M,F)
*     .. Scalar Arguments ..
      DOUBLE PRECISION X
      INTEGER         NEQ
*     .. Array Arguments ..
      DOUBLE PRECISION F(NEQ), Y(NEQ,0:*)
      INTEGER         M(NEQ)
*     .. Scalars in Common ..
      DOUBLE PRECISION BETA0, ETA, LAMBDA, MU, PI
*     .. Local Scalars ..
      DOUBLE PRECISION BETA
*     .. Intrinsic Functions ..
      INTRINSIC       COS
*     .. Common blocks ..
      COMMON          /PROB/ETA, MU, LAMBDA, BETA0, PI
*     .. Executable Statements ..
      BETA = BETA0*(1.0D0+COS(2.0D0*PI*X))
      F(1) = MU - BETA*Y(1,0)*Y(3,0)
      F(2) = BETA*Y(1,0)*Y(3,0) - Y(2,0)/LAMBDA
      F(3) = Y(2,0)/LAMBDA - Y(3,0)/ETA
      F(4) = 0.0D0
      F(5) = 0.0D0
      F(6) = 0.0D0
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
      DOUBLE PRECISION BETA0, ETA, LAMBDA, MU, PI
*     .. Local Scalars ..
      DOUBLE PRECISION BETA
*     .. Intrinsic Functions ..
      INTRINSIC       COS
*     .. Common blocks ..
      COMMON          /PROB/ETA, MU, LAMBDA, BETA0, PI
*     .. Executable Statements ..
      BETA = BETA0*(1.0D0+COS(2.0D0*PI*X))
      DF(1,1,0) = -BETA*Y(3,0)
      DF(1,3,0) = -BETA*Y(1,0)
      DF(2,1,0) = BETA*Y(3,0)
      DF(2,2,0) = -1.0D0/LAMBDA
      DF(2,3,0) = BETA*Y(1,0)
      DF(3,2,0) = 1.0D0/LAMBDA
      DF(3,3,0) = -1.0D0/ETA
      RETURN
      END
      SUBROUTINE GAFUN(YA,NEQ,M,NLBC,GA)
*     .. Scalar Arguments ..
      INTEGER          NEQ, NLBC
*     .. Array Arguments ..
      DOUBLE PRECISION GA(NLBC), YA(NEQ,0:*)
      INTEGER          M(NEQ)
*     .. Executable Statements ..
      GA(1) = YA(1,0) - YA(4,0)
      GA(2) = YA(2,0) - YA(5,0)
      GA(3) = YA(3,0) - YA(6,0)
      RETURN
      END
      SUBROUTINE GBFUN(YB,NEQ,M,NRBC,GB)
*     .. Scalar Arguments ..
      INTEGER          NEQ, NRBC
*     .. Array Arguments ..
      DOUBLE PRECISION GB(NRBC), YB(NEQ,0:*)
      INTEGER          M(NEQ)
*     .. Executable Statements ..
      GB(1) = YB(1,0) - YB(4,0)
      GB(2) = YB(2,0) - YB(5,0)
      GB(3) = YB(3,0) - YB(6,0)
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
      DGA(1,4,0) = -1.0D0
      DGA(2,2,0) = 1.0D0
      DGA(2,5,0) = -1.0D0
      DGA(3,3,0) = 1.0D0
      DGA(3,6,0) = -1.0D0
      RETURN
      END
      SUBROUTINE GBJAC(YB,NEQ,M,NRBC,DGB)
*     .. Scalar Arguments ..
      INTEGER          NEQ, NRBC
*     .. Array Arguments ..
      DOUBLE PRECISION DGB(NRBC,NEQ,0:*), YB(NEQ,0:*)
      INTEGER          M(NEQ)
*     .. Executable Statements ..
      DGB(1,1,0) = 1.0D0
      DGB(1,4,0) = -1.0D0
      DGB(2,2,0) = 1.0D0
      DGB(2,5,0) = -1.0D0
      DGB(3,3,0) = 1.0D0
      DGB(3,6,0) = -1.0D0
      RETURN
      END
      SUBROUTINE GUESS(X,NEQ,M,Z,DMVAL)
*     .. Scalar Arguments ..
      DOUBLE PRECISION X
      INTEGER          NEQ
*     .. Array Arguments ..
      DOUBLE PRECISION DMVAL(NEQ), Z(NEQ,0:*)
      INTEGER          M(NEQ)
*     .. Local Scalars ..
      INTEGER          I
*     .. Executable Statements ..
      Z(1,0) = 1.0D0
      Z(2,0) = 1.0D0
      Z(3,0) = 1.0D0
      Z(4,0) = Z(1,0)
      Z(5,0) = Z(2,0)
      Z(6,0) = Z(3,0)
      DO 20 I = 1, NEQ
         DMVAL(I) = 0.0D0
   20 CONTINUE
      RETURN
      END
