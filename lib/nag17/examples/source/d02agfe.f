*     D02AGF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. External Subroutines ..
      EXTERNAL         EX1, EX2
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D02AGF Example Program Results'
      CALL EX1
      CALL EX2
      STOP
      END
*
      SUBROUTINE EX1
*     .. Parameters ..
      INTEGER          N, M1
      PARAMETER        (N=2,M1=6)
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Scalars in Common ..
      INTEGER          IPRINT
*     .. Local Scalars ..
      DOUBLE PRECISION DUM, H, R, X, X1
      INTEGER          I, IFAIL, J, N1
*     .. Local Arrays ..
      DOUBLE PRECISION C(M1,N), COPY(N,N), ERROR(N), G(N), G1(N),
     +                 MAT(N,N), PARAM(N), PARERR(N), WSPACE(N,9)
*     .. External Subroutines ..
      EXTERNAL         AUX1, BCAUX1, D02AGF, PRSOL, RNAUX1
*     .. Intrinsic Functions ..
      INTRINSIC        DBLE
*     .. Common blocks ..
      COMMON           /BLOCK1/IPRINT
*     .. Executable Statements ..
      WRITE (NOUT,*)
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Case 1'
      WRITE (NOUT,*)
*     * Set IPRINT to 1 to obtain output from PRSOL at each iteration *
      IPRINT = 0
      PARAM(1) = 0.2D0
      PARAM(2) = 0.0D0
      N1 = 2
      H = 0.1D0
      PARERR(1) = 1.0D-5
      PARERR(2) = 1.0D-3
      ERROR(1) = 1.0D-4
      ERROR(2) = 1.0D-4
      IFAIL = 1
*
      CALL D02AGF(H,ERROR,PARERR,PARAM,C,N,N1,M1,AUX1,BCAUX1,RNAUX1,
     +            PRSOL,MAT,COPY,WSPACE,G,G1,IFAIL)
*
      IF (IFAIL.EQ.0) THEN
         WRITE (NOUT,*) 'Final parameters'
         WRITE (NOUT,99998) (PARAM(I),I=1,N1)
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Final solution'
         WRITE (NOUT,*) 'X-value     Components of solution'
         CALL RNAUX1(X,X1,R,PARAM)
         H = (X1-X)/5.0D0
         DO 20 I = 1, 6
            DUM = X + DBLE(I-1)*H
            WRITE (NOUT,99997) DUM, (C(I,J),J=1,N)
   20    CONTINUE
      ELSE
         WRITE (NOUT,99999) 'IFAIL = ', IFAIL
      END IF
      RETURN
*
99999 FORMAT (1X,A,I3)
99998 FORMAT (1X,3D16.6)
99997 FORMAT (1X,F7.2,3D13.4)
      END
*
      SUBROUTINE AUX1(F,Y,X,PARAM)
*     .. Scalar Arguments ..
      DOUBLE PRECISION X
*     .. Array Arguments ..
      DOUBLE PRECISION F(2), PARAM(2), Y(2)
*     .. Executable Statements ..
      F(1) = Y(2)
      F(2) = (Y(1)**3-Y(2))/(2.0D0*X)
      RETURN
      END
*
      SUBROUTINE RNAUX1(X,X1,R,PARAM)
*     .. Scalar Arguments ..
      DOUBLE PRECISION  R, X, X1
*     .. Array Arguments ..
      DOUBLE PRECISION  PARAM(2)
*     .. Executable Statements ..
      X = 0.1D0
      X1 = 16.0D0
      R = 16.0D0
      RETURN
      END
*
      SUBROUTINE BCAUX1(G,G1,PARAM)
*     .. Array Arguments ..
      DOUBLE PRECISION  G(2), G1(2), PARAM(2)
*     .. Local Scalars ..
      DOUBLE PRECISION  Z
*     .. Intrinsic Functions ..
      INTRINSIC         SQRT
*     .. Executable Statements ..
      Z = 0.1D0
      G(1) = 0.1D0 + PARAM(1)*SQRT(Z)*0.1D0 + 0.01D0*Z
      G(2) = PARAM(1)*0.05D0/SQRT(Z) + 0.01D0
      G1(1) = 1.0D0/6.0D0
      G1(2) = PARAM(2)
      RETURN
      END
*
      SUBROUTINE EX2
*     .. Parameters ..
      INTEGER          N, M1
      PARAMETER        (N=3,M1=6)
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Scalars in Common ..
      INTEGER          IPRINT
*     .. Local Scalars ..
      DOUBLE PRECISION DUM, H, R, X, X1
      INTEGER          I, IFAIL, J
*     .. Local Arrays ..
      DOUBLE PRECISION C(M1,N), COPY(N,N), ERROR(N), G(N), G1(N),
     +                 MAT(N,N), PARAM(N), PARERR(N), WSPACE(N,9)
*     .. External Subroutines ..
      EXTERNAL         AUX2, BCAUX2, D02AGF, PRSOL, RNAUX2
*     .. Intrinsic Functions ..
      INTRINSIC        DBLE
*     .. Common blocks ..
      COMMON           /BLOCK1/IPRINT
*     .. Executable Statements ..
      WRITE (NOUT,*)
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Case 2'
      WRITE (NOUT,*)
*     * Set IPRINT to 1 to obtain output from PRSOL at each iteration *
      IPRINT = 0
      H = 10.0D0
      PARAM(1) = 32.0D0
      PARAM(2) = 6000.0D0
      PARAM(3) = 0.54D0
      PARERR(1) = 1.0D-5
      PARERR(2) = 1.0D-4
      PARERR(3) = 1.0D-4
      ERROR(1) = 1.0D-2
      ERROR(2) = 1.0D-2
      ERROR(3) = 1.0D-2
      IFAIL = 1
*
      CALL D02AGF(H,ERROR,PARERR,PARAM,C,N,N,M1,AUX2,BCAUX2,RNAUX2,
     +            PRSOL,MAT,COPY,WSPACE,G,G1,IFAIL)
*
      IF (IFAIL.EQ.0) THEN
         WRITE (NOUT,*) 'Final parameters'
         WRITE (NOUT,99998) (PARAM(I),I=1,N)
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Final solution'
         WRITE (NOUT,*) 'X-value     Components of solution'
         CALL RNAUX2(X,X1,R,PARAM)
         H = (X1-X)/5.0D0
         DO 20 I = 1, 6
            DUM = X + DBLE(I-1)*H
            WRITE (NOUT,99997) DUM, (C(I,J),J=1,N)
   20    CONTINUE
      ELSE
         WRITE (NOUT,99999) 'IFAIL = ', IFAIL
      END IF
      RETURN
*
99999 FORMAT (1X,A,I3)
99998 FORMAT (1X,3D16.6)
99997 FORMAT (1X,F7.0,3D13.4)
      END
*
      SUBROUTINE AUX2(F,Y,X,PARAM)
*     .. Scalar Arguments ..
      DOUBLE PRECISION X
*     .. Array Arguments ..
      DOUBLE PRECISION F(3), PARAM(3), Y(3)
*     .. Local Scalars ..
      DOUBLE PRECISION C, S
*     .. Intrinsic Functions ..
      INTRINSIC       COS, SIN
*     .. Executable Statements ..
      C = COS(Y(3))
      S = SIN(Y(3))
      F(1) = S/C
      F(2) = -(PARAM(1)*S+0.00002D0*Y(2)*Y(2))/(Y(2)*C)
      F(3) = -PARAM(1)/(Y(2)*Y(2))
      RETURN
      END
*
      SUBROUTINE RNAUX2(X,X1,R,PARAM)
*     .. Scalar Arguments ..
      DOUBLE PRECISION  R, X, X1
*     .. Array Arguments ..
      DOUBLE PRECISION  PARAM(3)
*     .. Executable Statements ..
      X = 0.0D0
      X1 = PARAM(2)
      R = PARAM(2)
      RETURN
      END
*
      SUBROUTINE BCAUX2(G,G1,PARAM)
*     .. Array Arguments ..
      DOUBLE PRECISION  G(3), G1(3), PARAM(3)
*     .. Executable Statements ..
      G(1) = 0.0D0
      G(2) = 500.0D0
      G(3) = 0.5D0
      G1(1) = 0.0D0
      G1(2) = 450.0D0
      G1(3) = PARAM(3)
      RETURN
      END
*
      SUBROUTINE PRSOL(PARAM,RESID,N1,ERR)
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Scalar Arguments ..
      DOUBLE PRECISION RESID
      INTEGER          N1
*     .. Array Arguments ..
      DOUBLE PRECISION ERR(N1), PARAM(N1)
*     .. Scalars in Common ..
      INTEGER          IPRINT
*     .. Local Scalars ..
      INTEGER          I
*     .. Common blocks ..
      COMMON           /BLOCK1/IPRINT
*     .. Executable Statements ..
      IF (IPRINT.NE.0) THEN
         WRITE (NOUT,99999) 'Current parameters   ', (PARAM(I),I=1,N1)
         WRITE (NOUT,99998) 'Residuals   ', (ERR(I),I=1,N1)
         WRITE (NOUT,99998) 'Sum of residuals squared  ', RESID
         WRITE (NOUT,*)
      END IF
      RETURN
*
99999 FORMAT (1X,A,6(D14.6,2X))
99998 FORMAT (1X,A,6(D12.4,1X))
      END
