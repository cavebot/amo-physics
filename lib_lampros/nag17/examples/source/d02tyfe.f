*     D02TYF Example Program Text
*     Mark 17 Release. NAG Copyright 1995.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          NEQ, MMAX, NLBC, NRBC, NCOL, MXMESH
      PARAMETER        (NEQ=1,MMAX=2,NLBC=1,NRBC=1,NCOL=4,MXMESH=100)
      INTEGER          LRWORK, LIWORK
      PARAMETER        (LRWORK=MXMESH*(109*NEQ**2+78*NEQ+7),
     +                 LIWORK=MXMESH*(11*NEQ+6))
*     .. Scalars in Common ..
      DOUBLE PRECISION A
*     .. Local Scalars ..
      DOUBLE PRECISION AINC, ERMX, XX
      INTEGER          I, IERMX, IFAIL, IJERMX, NMESH
      LOGICAL          FAILED
*     .. Local Arrays ..
      DOUBLE PRECISION MESH(MXMESH), TOL(NEQ), WORK(LRWORK),
     +                 Y(NEQ,0:MMAX-1)
      INTEGER          IPMESH(MXMESH), IWORK(LIWORK), M(NEQ)
*     .. External Subroutines ..
      EXTERNAL         D02TKF, D02TVF, D02TYF, D02TZF, FFUN, FJAC,
     +                 GAFUN, GAJAC, GBFUN, GBJAC, GUESS
*     .. Intrinsic Functions ..
      INTRINSIC        DBLE
*     .. Common blocks ..
      COMMON           /PROBS/A
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D02TYF Example Program Results'
      WRITE (NOUT,*)
      A = 1.0D0
      NMESH = 6
      AINC = A/DBLE(NMESH-1)
      MESH(1) = 0.0D0
      IPMESH(1) = 1
      DO 20 I = 2, NMESH - 1
         MESH(I) = DBLE(I-1)*AINC
         IPMESH(I) = 2
   20 CONTINUE
      MESH(NMESH) = A
      IPMESH(NMESH) = 1
      TOL(1) = 1.0D-5
      M(1) = 2
      IFAIL = 0
      CALL D02TVF(NEQ,M,NLBC,NRBC,NCOL,TOL,MXMESH,NMESH,MESH,IPMESH,
     +            WORK,LRWORK,IWORK,LIWORK,IFAIL)
      WRITE (NOUT,99997) TOL(1), A
      IFAIL = -1
      CALL D02TKF(FFUN,FJAC,GAFUN,GBFUN,GAJAC,GBJAC,GUESS,WORK,IWORK,
     +            IFAIL)
      FAILED = IFAIL .NE. 0
      IFAIL = 0
      CALL D02TZF(MXMESH,NMESH,MESH,IPMESH,ERMX,IERMX,IJERMX,WORK,IWORK,
     +            IFAIL)
      WRITE (NOUT,99996) NMESH, ERMX, IERMX, IJERMX,
     +  (I,IPMESH(I),MESH(I),I=1,NMESH)
      IF ( .NOT. FAILED) THEN
         AINC = 0.1D0*A
         WRITE (NOUT,99999)
         DO 40 I = 1, 11
            XX = DBLE(I-1)*AINC
            CALL D02TYF(XX,Y,NEQ,MMAX,WORK,IWORK,IFAIL)
            WRITE (NOUT,99998) XX, Y(1,0), Y(1,1)
   40    CONTINUE
      END IF
      STOP
*
99999 FORMAT (/' Computed solution',/'       x     solution   derivati',
     +       've')
99998 FORMAT (' ',F8.2,2F11.5)
99997 FORMAT (//' Tolerance = ',D8.1,' A = ',F8.2)
99996 FORMAT (/' Used a mesh of ',I4,' points',/' Maximum error = ',
     +       D10.2,'  in interval ',I4,' for component ',I4,//' Mesh p',
     +       'oints:',/4(I4,'(',I1,')',D11.4))
      END
      SUBROUTINE FFUN(X,Y,NEQ,M,F)
*     .. Scalar Arguments ..
      DOUBLE PRECISION X
      INTEGER         NEQ
*     .. Array Arguments ..
      DOUBLE PRECISION F(NEQ), Y(NEQ,0:*)
      INTEGER         M(NEQ)
*     .. Intrinsic Functions ..
      INTRINSIC       SQRT
*     .. Executable Statements ..
      IF (Y(1,0).LE.0.0D0) THEN
         F(1) = 0.0D0
         PRINT *, ' F'
      ELSE
         F(1) = (Y(1,0))**1.5D0/SQRT(X)
      END IF
      RETURN
      END
      SUBROUTINE FJAC(X,Y,NEQ,M,DF)
*     .. Scalar Arguments ..
      DOUBLE PRECISION X
      INTEGER         NEQ
*     .. Array Arguments ..
      DOUBLE PRECISION DF(NEQ,NEQ,0:*), Y(NEQ,0:*)
      INTEGER         M(NEQ)
*     .. Intrinsic Functions ..
      INTRINSIC       SQRT
*     .. Executable Statements ..
      IF (Y(1,0).LE.0.0D0) THEN
         DF(1,1,0) = 0.0D0
         PRINT *, ' JAC'
      ELSE
         DF(1,1,0) = 1.5D0*SQRT(Y(1,0))/SQRT(X)
      END IF
      RETURN
      END
      SUBROUTINE GAFUN(YA,NEQ,M,NLBC,GA)
*     .. Scalar Arguments ..
      INTEGER          NEQ, NLBC
*     .. Array Arguments ..
      DOUBLE PRECISION GA(NLBC), YA(NEQ,0:*)
      INTEGER          M(NEQ)
*     .. Executable Statements ..
      GA(1) = YA(1,0) - 1.0D0
      RETURN
      END
      SUBROUTINE GBFUN(YB,NEQ,M,NRBC,GB)
*     .. Scalar Arguments ..
      INTEGER          NEQ, NRBC
*     .. Array Arguments ..
      DOUBLE PRECISION GB(NRBC), YB(NEQ,0:*)
      INTEGER          M(NEQ)
*     .. Executable Statements ..
      GB(1) = YB(1,0)
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
      DOUBLE PRECISION A
*     .. Common blocks ..
      COMMON           /PROBS/A
*     .. Executable Statements ..
      Z(1,0) = 1.0D0 - X/A
      Z(1,1) = -1.0D0/A
      DMVAL(1) = 0.0D0
      RETURN
      END
