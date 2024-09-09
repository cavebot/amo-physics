      SUBROUTINE Y90SCF(MATRIX,M,MN,N,KL,KU,A,LDA,B,LDB,C,LDC)
C     MARK 15 RE-ISSUE. NAG COPYRIGHT 1991.
C-----------------------------------------------------------------------
C
C         ==============================================================
C         *  Y90SCF  Multiplication of a Band by a Rectangular Matrix  *
C         ==============================================================
C
C-----------------------------------------------------------------------
C     .. Parameters ..
      DOUBLE PRECISION  ZERO
      PARAMETER         (ZERO=0.0D0)
C     .. Scalar Arguments ..
      INTEGER           KL, KU, LDA, LDB, LDC, M, MN, N
      CHARACTER*1       MATRIX
C     .. Array Arguments ..
      DOUBLE PRECISION  A(LDA,*), B(LDB,*), C(LDC,*)
C     .. Local Scalars ..
      DOUBLE PRECISION  TEMP
      INTEGER           I, J, K, LL, LU
C     .. External Functions ..
      LOGICAL           Y90WAF
      EXTERNAL          Y90WAF
C     .. Intrinsic Functions ..
      INTRINSIC         MAX, MIN
C     .. Executable Statements ..
C-----------------------------------------------------------------------
C
C     Initialise
C
C-----------------------------------------------------------------------
      DO 40 J = 1, N
         DO 20 I = 1, M
            C(I,J) = ZERO
   20    CONTINUE
   40 CONTINUE
      LL = KL
      LU = KU
      IF (Y90WAF(MATRIX,'L')) THEN
         LU = 0
      ELSE IF (Y90WAF(MATRIX,'U')) THEN
         LL = 0
      END IF
C-----------------------------------------------------------------------
C
C     Symmetric banded matrix
C
C-----------------------------------------------------------------------
C
C     Stored in lower triangular part
C
      IF (Y90WAF(MATRIX,'S')) THEN
         DO 100 J = 1, N
            DO 80 K = 1, MN
               TEMP = B(K,J)
               DO 60 I = K + 1, MIN(M,K+LL)
                  C(I,J) = C(I,J) + TEMP*A(I,LL+K+1-I)
                  C(K,J) = C(K,J) + A(I,LL+K+1-I)*B(I,J)
   60          CONTINUE
               C(K,J) = C(K,J) + A(K,LL+1)*TEMP
   80       CONTINUE
  100    CONTINUE
C
C     Stored in upper triangular part
C
      ELSE IF (Y90WAF(MATRIX,'Y')) THEN
         DO 160 J = 1, N
            DO 140 K = 1, MN
               TEMP = B(K,J)
               DO 120 I = MAX(1,K-LU), K - 1
                  C(I,J) = C(I,J) + TEMP*A(I,K+1-I)
                  C(K,J) = C(K,J) + A(I,K+1-I)*B(I,J)
  120          CONTINUE
               C(K,J) = C(K,J) + A(K,1)*TEMP
  140       CONTINUE
  160    CONTINUE
C-----------------------------------------------------------------------
C
C     General or triangular banded matrix
C
C-----------------------------------------------------------------------
      ELSE
         DO 220 J = 1, N
            DO 200 K = 1, MN
               TEMP = B(K,J)
               DO 180 I = MAX(1,K-LU), MIN(M,K+LL)
                  C(I,J) = C(I,J) + TEMP*A(I,LL+K+1-I)
  180          CONTINUE
  200       CONTINUE
  220    CONTINUE
      END IF
C-----------------------------------------------------------------------
C
C     End of Y90SCF
C
C-----------------------------------------------------------------------
      RETURN
      END
