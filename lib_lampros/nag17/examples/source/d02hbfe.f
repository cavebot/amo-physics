*     D02HBF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. External Subroutines ..
      EXTERNAL         EX1, EX2
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D02HBF Example Program Results'
      CALL EX1
      CALL EX2
      STOP
      END
*
      SUBROUTINE EX1
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          N, N1, IW, M1
      PARAMETER        (N=2,N1=2,IW=3*N+14+11,M1=6)
*     .. Local Scalars ..
      DOUBLE PRECISION H, X, X1
      INTEGER          I, IFAIL, J
*     .. Local Arrays ..
      DOUBLE PRECISION C(N,M1), ERROR(N), PARAM(N1), PARERR(N1), W(N,IW)
*     .. External Subroutines ..
      EXTERNAL         AUX1, BCAUX1, D02HBF, RNAUX1, X04ABF
*     .. Intrinsic Functions ..
      INTRINSIC        DBLE
*     .. Executable Statements ..
      WRITE (NOUT,*)
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Case 1'
      CALL X04ABF(1,NOUT)
      PARAM(1) = 0.2D0
      PARAM(2) = 0.0D0
      PARERR(1) = 1.0D-5
      PARERR(2) = 1.0D-3
      ERROR(1) = 1.0D-4
      ERROR(2) = 1.0D-4
*     * Set IFAIL to 111 to obtain monitoring information *
      IFAIL = 11
*
      CALL D02HBF(PARAM,N1,PARERR,ERROR,N,C,M1,AUX1,BCAUX1,RNAUX1,W,IW,
     +            IFAIL)
*
      WRITE (NOUT,*)
      IF (IFAIL.NE.0) THEN
         WRITE (NOUT,99999) 'IFAIL = ', IFAIL
         IF (IFAIL.LE.5 .AND. IFAIL.NE.1) THEN
            WRITE (NOUT,*)
            WRITE (NOUT,99996) 'W(1,2) = ', W(1,2), ' W(.,1) = ',
     +        (W(I,1),I=1,N)
         END IF
      ELSE
         WRITE (NOUT,*) 'Final parameters'
         WRITE (NOUT,99998) (PARAM(I),I=1,N1)
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Final solution'
         WRITE (NOUT,*) 'X-value     Components of solution'
         CALL RNAUX1(X,X1,PARAM)
         H = (X1-X)/DBLE(M1-1)
         DO 20 I = 1, M1
            WRITE (NOUT,99997) X + (I-1)*H, (C(J,I),J=1,N)
   20    CONTINUE
      END IF
      RETURN
*
99999 FORMAT (1X,A,I3)
99998 FORMAT (1X,1P,3D15.3)
99997 FORMAT (1X,F7.2,2F13.4)
99996 FORMAT (1X,A,F9.4,A,10D10.3)
      END
*
      SUBROUTINE AUX1(X,Y,F,PARAM)
*     .. Parameters ..
      INTEGER         N
      PARAMETER       (N=2)
*     .. Scalar Arguments ..
      DOUBLE PRECISION X
*     .. Array Arguments ..
      DOUBLE PRECISION F(N), PARAM(N), Y(N)
*     .. Executable Statements ..
      F(1) = Y(2)
      F(2) = (Y(1)**3-Y(2))/(2.0D0*X)
      RETURN
      END
*
      SUBROUTINE RNAUX1(X,X1,PARAM)
*     .. Scalar Arguments ..
      DOUBLE PRECISION  X, X1
*     .. Array Arguments ..
      DOUBLE PRECISION  PARAM(2)
*     .. Executable Statements ..
      X = 0.1D0
      X1 = 16.0D0
      RETURN
      END
*
      SUBROUTINE BCAUX1(G,G1,PARAM)
*     .. Parameters ..
      INTEGER           N
      PARAMETER         (N=2)
*     .. Array Arguments ..
      DOUBLE PRECISION  G(N), G1(N), PARAM(N)
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
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          N, N1, IW, M1
      PARAMETER        (N=3,N1=3,IW=3*N+14+11,M1=6)
*     .. Local Scalars ..
      DOUBLE PRECISION H, X, X1
      INTEGER          I, IFAIL, J
*     .. Local Arrays ..
      DOUBLE PRECISION C(N,M1), ERROR(N), PARAM(N1), PARERR(N1), W(N,IW)
*     .. External Subroutines ..
      EXTERNAL         AUX2, BCAUX2, D02HBF, RNAUX2, X04ABF
*     .. Intrinsic Functions ..
      INTRINSIC        DBLE
*     .. Executable Statements ..
      WRITE (NOUT,*)
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Case 2'
      CALL X04ABF(1,NOUT)
      PARAM(1) = 32.0D0
      PARAM(2) = 6000.0D0
      PARAM(3) = 0.54D0
      PARERR(1) = 1.0D-5
      PARERR(2) = 1.0D-4
      PARERR(3) = 1.0D-4
      ERROR(1) = 1.0D-2
      ERROR(2) = 1.0D-2
      ERROR(3) = 1.0D-2
*     * Set IFAIL to 111 to obtain monitoring information *
      IFAIL = 11
*
      CALL D02HBF(PARAM,N1,PARERR,ERROR,N,C,M1,AUX2,BCAUX2,RNAUX2,W,IW,
     +            IFAIL)
*
      WRITE (NOUT,*)
      IF (IFAIL.NE.0) THEN
         WRITE (NOUT,99999) 'IFAIL = ', IFAIL
         IF (IFAIL.LE.5 .AND. IFAIL.NE.1) THEN
            WRITE (NOUT,*)
            WRITE (NOUT,99996) 'W(1,2) = ', W(1,2), ' W(.,1) = ',
     +        (W(I,1),I=1,N)
         END IF
      ELSE
         WRITE (NOUT,*) 'Final parameters'
         WRITE (NOUT,99998) (PARAM(I),I=1,N1)
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Final solution'
         WRITE (NOUT,*) 'X-value     Components of solution'
         CALL RNAUX2(X,X1,PARAM)
         H = (X1-X)/DBLE(M1-1)
         DO 20 I = 1, M1
            WRITE (NOUT,99997) X + (I-1)*H, (C(J,I),J=1,N)
   20    CONTINUE
      END IF
      RETURN
*
99999 FORMAT (1X,A,I3)
99998 FORMAT (1X,1P,3D15.3)
99997 FORMAT (1X,F7.0,2F13.1,F13.3)
99996 FORMAT (1X,A,F9.4,A,10D10.3)
      END
*
      SUBROUTINE AUX2(X,Y,F,PARAM)
*     .. Parameters ..
      INTEGER         N
      PARAMETER       (N=3)
*     .. Scalar Arguments ..
      DOUBLE PRECISION X
*     .. Array Arguments ..
      DOUBLE PRECISION F(N), PARAM(N), Y(N)
*     .. Intrinsic Functions ..
      INTRINSIC       COS, TAN
*     .. Executable Statements ..
      F(1) = TAN(Y(3))
      F(2) = -PARAM(1)*TAN(Y(3))/Y(2) - 0.00002D0*Y(2)/COS(Y(3))
      F(3) = -PARAM(1)/Y(2)**2
      RETURN
      END
*
      SUBROUTINE RNAUX2(X,X1,PARAM)
*     .. Parameters ..
      INTEGER           N
      PARAMETER         (N=3)
*     .. Scalar Arguments ..
      DOUBLE PRECISION  X, X1
*     .. Array Arguments ..
      DOUBLE PRECISION  PARAM(N)
*     .. Executable Statements ..
      X = 0.0D0
      X1 = PARAM(2)
      RETURN
      END
*
      SUBROUTINE BCAUX2(G,G1,PARAM)
*     .. Parameters ..
      INTEGER           N
      PARAMETER         (N=3)
*     .. Array Arguments ..
      DOUBLE PRECISION  G(N), G1(N), PARAM(N)
*     .. Executable Statements ..
      G(1) = 0.0D0
      G(2) = 500.0D0
      G(3) = 0.5D0
      G1(1) = 0.0D0
      G1(2) = 450.0D0
      G1(3) = PARAM(3)
      RETURN
      END
