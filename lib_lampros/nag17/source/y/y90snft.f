      SUBROUTINE Y90SNF(MATRA,MATRB,M,N,MN,ALPHA,A,IA,B,IB,BETA,C,IC)
C     MARK 15 RE-ISSUE. NAG COPYRIGHT 1991.
C-----------------------------------------------------------------------
C
C         ================================================
C         *  Y90SNF :  Triangular Matrix Multiplication  *
C         ================================================
C
C-----------------------------------------------------------------------
C     .. Parameters ..
      DOUBLE PRECISION  ZERO
      PARAMETER         (ZERO=0.0D0)
C     .. Scalar Arguments ..
      DOUBLE PRECISION  ALPHA, BETA
      INTEGER           IA, IB, IC, M, MN, N
      CHARACTER*1       MATRA, MATRB
C     .. Array Arguments ..
      DOUBLE PRECISION  A(IA,*), B(IB,*), C(IC,*)
C     .. Local Scalars ..
      DOUBLE PRECISION  TEMP
      INTEGER           I, J, K
C     .. External Functions ..
      LOGICAL           Y90WAF
      EXTERNAL          Y90WAF
C     .. Intrinsic Functions ..
      INTRINSIC         MIN
C     .. Executable Statements ..
C-----------------------------------------------------------------------
C
C     Initialize C
C
C-----------------------------------------------------------------------
      DO 60 J = 1, N
         IF (BETA.EQ.ZERO) THEN
            DO 20 I = 1, M
               C(I,J) = ZERO
   20       CONTINUE
         ELSE
            DO 40 I = 1, M
               C(I,J) = BETA*C(I,J)
   40       CONTINUE
         END IF
   60 CONTINUE
C-----------------------------------------------------------------------
C
C     A is lower triangular
C
C-----------------------------------------------------------------------
      IF (Y90WAF(MATRA,'L')) THEN
C
C     B is upper triangular
C
         IF (Y90WAF(MATRB,'U')) THEN
            DO 120 J = 1, N
               DO 100 K = 1, MIN(M,J)
                  TEMP = ALPHA*B(K,J)
                  DO 80 I = K, M
                     C(I,J) = C(I,J) + TEMP*A(I,K)
   80             CONTINUE
  100          CONTINUE
  120       CONTINUE
C
C     B is lower triangular
C
         ELSE
            DO 180 J = 1, M
               DO 160 K = J, M
                  TEMP = ALPHA*B(K,J)
                  DO 140 I = K, M
                     C(I,J) = C(I,J) + TEMP*A(I,K)
  140             CONTINUE
  160          CONTINUE
  180       CONTINUE
         END IF
C-----------------------------------------------------------------------
C
C     A is upper triangular
C
C-----------------------------------------------------------------------
      ELSE
C
C     B is upper triangular
C
         IF (Y90WAF(MATRB,'U')) THEN
            DO 240 J = 1, N
               DO 220 K = 1, J
                  TEMP = ALPHA*B(K,J)
                  DO 200 I = 1, K
                     C(I,J) = C(I,J) + TEMP*A(I,K)
  200             CONTINUE
  220          CONTINUE
  240       CONTINUE
C
C     B is lower triangular
C
         ELSE
            DO 300 J = 1, MN
               DO 280 K = J, MN
                  TEMP = ALPHA*B(K,J)
                  DO 260 I = 1, K
                     C(I,J) = C(I,J) + TEMP*A(I,K)
  260             CONTINUE
  280          CONTINUE
  300       CONTINUE
         END IF
      END IF
C-----------------------------------------------------------------------
C
C     End of Y90SNF
C
C-----------------------------------------------------------------------
      RETURN
      END
