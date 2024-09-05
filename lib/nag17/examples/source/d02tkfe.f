*     D02TKF Example Program Text
*     Mark 17 Release. NAG Copyright 1995.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          NEQ, MMAX, NLBC, NRBC, NCOL, MXMESH
      PARAMETER        (NEQ=3,MMAX=3,NLBC=3,NRBC=3,NCOL=7,MXMESH=51)
      INTEGER          LRWORK, LIWORK
      PARAMETER        (LRWORK=MXMESH*(109*NEQ**2+78*NEQ+7),
     +                 LIWORK=MXMESH*(11*NEQ+6))
*     .. Scalars in Common ..
      DOUBLE PRECISION OMEGA, SQROFR
*     .. Local Scalars ..
      DOUBLE PRECISION ERMX, R
      INTEGER          I, IERMX, IFAIL, IJERMX, J, NCONT, NMESH
*     .. Local Arrays ..
      DOUBLE PRECISION MESH(MXMESH), TOL(NEQ), WORK(LRWORK),
     +                 Y(NEQ,0:MMAX-1)
      INTEGER          IPMESH(MXMESH), IWORK(LIWORK), M(NEQ)
*     .. External Subroutines ..
      EXTERNAL         D02TKF, D02TVF, D02TXF, D02TYF, D02TZF, FFUN,
     +                 FJAC, GAFUN, GAJAC, GBFUN, GBJAC, GUESS
*     .. Intrinsic Functions ..
      INTRINSIC        DBLE, SQRT
*     .. Common blocks ..
      COMMON           /PROBS/SQROFR, OMEGA
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D02TKF Example Program Results'
      WRITE (NOUT,*)
      NMESH = 11
      MESH(1) = 0.0D0
      IPMESH(1) = 1
      DO 20 I = 2, NMESH - 1
         MESH(I) = (I-1)/DBLE(NMESH-1)
         IPMESH(I) = 2
   20 CONTINUE
      MESH(NMESH) = 1.0D0
      IPMESH(NMESH) = 1
      M(1) = 1
      M(2) = 3
      M(3) = 2
      TOL(1) = 1.0D-4
      TOL(2) = TOL(1)
      TOL(3) = TOL(1)
      IFAIL = 0
      CALL D02TVF(NEQ,M,NLBC,NRBC,NCOL,TOL,MXMESH,NMESH,MESH,IPMESH,
     +            WORK,LRWORK,IWORK,LIWORK,IFAIL)
*     Initialize number of continuation steps
      NCONT = 3
*     Initialize problem dependent parameters
      OMEGA = 1.0D0
      R = 1.0D+6
      DO 80 J = 1, NCONT
         SQROFR = SQRT(R)
         WRITE (NOUT,99999) TOL(1), R
*     Solve
         CALL D02TKF(FFUN,FJAC,GAFUN,GBFUN,GAJAC,GBJAC,GUESS,WORK,IWORK,
     +               IFAIL)
*     Extract mesh
         CALL D02TZF(MXMESH,NMESH,MESH,IPMESH,ERMX,IERMX,IJERMX,WORK,
     +               IWORK,IFAIL)
         WRITE (NOUT,99998) NMESH, ERMX, IERMX, IJERMX,
     +     (I,IPMESH(I),MESH(I),I=1,NMESH)
*     Print solution components on mesh
         WRITE (NOUT,99997)
         DO 40 I = 1, NMESH
            CALL D02TYF(MESH(I),Y,NEQ,MMAX,WORK,IWORK,IFAIL)
            WRITE (NOUT,99996) MESH(I), Y(1,0), Y(2,0), Y(3,0)
   40    CONTINUE
*     Select mesh for continuation and modify problem dependent 
*     parameters
         IF (J.LT.NCONT) THEN
            R = 1.0D+02*R
            DO 60 I = 2, NMESH - 1
               IPMESH(I) = 2
   60       CONTINUE
            CALL D02TXF(MXMESH,NMESH,MESH,IPMESH,WORK,IWORK,IFAIL)
         END IF
   80 CONTINUE
      STOP
*
99999 FORMAT (/' Tolerance = ',1P,D8.1,'  R = ',D10.3)
99998 FORMAT (/' Used a mesh of ',I4,' points',/' Maximum error = ',
     +       D10.2,'  in interval ',I4,' for component ',I4,//' Mesh p',
     +       'oints:',/4(I4,'(',I1,')',D11.4))
99997 FORMAT (/'      x        f        f''       g')
99996 FORMAT (' ',F8.3,1X,3F9.4)
      END
      SUBROUTINE FFUN(X,Y,NEQ,M,F)
*     .. Scalar Arguments ..
      DOUBLE PRECISION X
      INTEGER         NEQ
*     .. Array Arguments ..
      DOUBLE PRECISION F(NEQ), Y(NEQ,0:*)
      INTEGER         M(NEQ)
*     .. Scalars in Common ..
      DOUBLE PRECISION OMEGA, SQROFR
*     .. Common blocks ..
      COMMON          /PROBS/SQROFR, OMEGA
*     .. Executable Statements ..
      F(1) = Y(2,0)
      F(2) = -(Y(1,0)*Y(2,2)+Y(3,0)*Y(3,1))*SQROFR
      F(3) = (Y(2,0)*Y(3,0)-Y(1,0)*Y(3,1))*SQROFR
      RETURN
      END
      SUBROUTINE FJAC(X,Y,NEQ,M,DFDY)
*     .. Scalar Arguments ..
      DOUBLE PRECISION X
      INTEGER         NEQ
*     .. Array Arguments ..
      DOUBLE PRECISION DFDY(NEQ,NEQ,0:*), Y(NEQ,0:*)
      INTEGER         M(NEQ)
*     .. Scalars in Common ..
      DOUBLE PRECISION OMEGA, SQROFR
*     .. Common blocks ..
      COMMON          /PROBS/SQROFR, OMEGA
*     .. Executable Statements ..
      DFDY(1,2,0) = 1.0D0
      DFDY(2,1,0) = -Y(2,2)*SQROFR
      DFDY(2,2,2) = -Y(1,0)*SQROFR
      DFDY(2,3,0) = -Y(3,1)*SQROFR
      DFDY(2,3,1) = -Y(3,0)*SQROFR
      DFDY(3,1,0) = -Y(3,1)*SQROFR
      DFDY(3,2,0) = Y(3,0)*SQROFR
      DFDY(3,3,0) = Y(2,0)*SQROFR
      DFDY(3,3,1) = -Y(1,0)*SQROFR
      RETURN
      END
      SUBROUTINE GAFUN(YA,NEQ,M,NLBC,GA)
*     .. Scalar Arguments ..
      INTEGER          NEQ, NLBC
*     .. Array Arguments ..
      DOUBLE PRECISION GA(NLBC), YA(NEQ,0:*)
      INTEGER          M(NEQ)
*     .. Scalars in Common ..
      DOUBLE PRECISION OMEGA, SQROFR
*     .. Common blocks ..
      COMMON           /PROBS/SQROFR, OMEGA
*     .. Executable Statements ..
      GA(1) = YA(1,0)
      GA(2) = YA(2,0)
      GA(3) = YA(3,0) - OMEGA
      RETURN
      END
      SUBROUTINE GBFUN(YB,NEQ,M,NRBC,GB)
*     .. Scalar Arguments ..
      INTEGER          NEQ, NRBC
*     .. Array Arguments ..
      DOUBLE PRECISION GB(NRBC), YB(NEQ,0:*)
      INTEGER          M(NEQ)
*     .. Scalars in Common ..
      DOUBLE PRECISION OMEGA, SQROFR
*     .. Common blocks ..
      COMMON           /PROBS/SQROFR, OMEGA
*     .. Executable Statements ..
      GB(1) = YB(1,0)
      GB(2) = YB(2,0)
      GB(3) = YB(3,0) + OMEGA
      RETURN
      END
      SUBROUTINE GAJAC(YA,NEQ,M,NLBC,DGADY)
*     .. Scalar Arguments ..
      INTEGER          NEQ, NLBC
*     .. Array Arguments ..
      DOUBLE PRECISION DGADY(NLBC,NEQ,0:*), YA(NEQ,0:*)
      INTEGER          M(NEQ)
*     .. Executable Statements ..
      DGADY(1,1,0) = 1.0D0
      DGADY(2,2,0) = 1.0D0
      DGADY(3,3,0) = 1.0D0
      RETURN
      END
      SUBROUTINE GBJAC(YB,NEQ,M,NRBC,DGBDY)
*     .. Scalar Arguments ..
      INTEGER          NEQ, NRBC
*     .. Array Arguments ..
      DOUBLE PRECISION DGBDY(NRBC,NEQ,0:*), YB(NEQ,0:*)
      INTEGER          M(NEQ)
*     .. Executable Statements ..
      DGBDY(1,1,0) = 1.0D0
      DGBDY(2,2,0) = 1.0D0
      DGBDY(3,3,0) = 1.0D0
      RETURN
      END
      SUBROUTINE GUESS(X,NEQ,M,Y,DYM)
*     .. Scalar Arguments ..
      DOUBLE PRECISION X
      INTEGER          NEQ
*     .. Array Arguments ..
      DOUBLE PRECISION DYM(NEQ), Y(NEQ,0:*)
      INTEGER          M(NEQ)
*     .. Scalars in Common ..
      DOUBLE PRECISION OMEGA, SQROFR
*     .. Common blocks ..
      COMMON           /PROBS/SQROFR, OMEGA
*     .. Executable Statements ..
      Y(1,0) = -X**2*(X-0.5D0)*(X-1.0D0)**2
      Y(2,0) = -X*(X-1.0D0)*(5.0D0*X**2-5.0D0*X+1.0D0)
      Y(2,1) = -(20.0D0*X**3-30.0D0*X**2+12.0D0*X-1.0D0)
      Y(2,2) = -(60.0D0*X**2-60.0D0*X+12.0D0*X)
      Y(3,0) = -8.0D0*OMEGA*(X-0.5D0)**3
      Y(3,1) = -24.0D0*OMEGA*(X-0.5D0)**2
      DYM(1) = Y(2,0)
      DYM(2) = -(120.0D0*X-60.0D0)
      DYM(3) = -56.0D0*OMEGA*(X-0.5D0)
      RETURN
      END
