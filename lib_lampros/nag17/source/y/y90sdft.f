      DOUBLE PRECISION FUNCTION Y90SDF(NORM,MATRIX,M,N,KL,KU,A,LDA)
C     MARK 15 RE-ISSUE. NAG COPYRIGHT 1991.
C-----------------------------------------------------------------------
C
C         =========================================
C         *  Y90SDF Norm of a Real Banded Matrix  *
C         =========================================
C
C-----------------------------------------------------------------------
C     .. Parameters ..
      DOUBLE PRECISION                 ZERO, ONE, TWO
      PARAMETER                        (ZERO=0.0D0,ONE=1.0D0,TWO=2.0D0)
C     .. Scalar Arguments ..
      INTEGER                          KL, KU, LDA, M, N
      CHARACTER*1                      MATRIX, NORM
C     .. Array Arguments ..
      DOUBLE PRECISION                 A(LDA,*)
C     .. Local Scalars ..
      DOUBLE PRECISION                 SCALE, SUM, VALUE
      INTEGER                          I, I1, J, LL, LU, NI
C     .. External Functions ..
      DOUBLE PRECISION                 F06BMF
      LOGICAL                          Y90WAF
      EXTERNAL                         F06BMF, Y90WAF
C     .. External Subroutines ..
      EXTERNAL                         F06FJF
C     .. Intrinsic Functions ..
      INTRINSIC                        ABS, MAX, MIN
C     .. Executable Statements ..
C-----------------------------------------------------------------------
C
C     Initialize
C
C-----------------------------------------------------------------------
      LL = KL
      LU = KU
      IF (Y90WAF(MATRIX,'L') .OR. Y90WAF(MATRIX,'S')) THEN
         LU = 0
      ELSE IF (Y90WAF(MATRIX,'U') .OR. Y90WAF(MATRIX,'Y')) THEN
         LL = 0
      END IF
      VALUE = ZERO
C-----------------------------------------------------------------------
C
C     Calculate Frobenius norm of A
C
C-----------------------------------------------------------------------
      IF (Y90WAF(NORM,'F')) THEN
         SCALE = ZERO
         SUM = ONE
C
         IF (Y90WAF(MATRIX,'S') .OR. Y90WAF(MATRIX,'L')) THEN
            DO 20 J = 1, LL
               I1 = LL + 2 - J
               NI = MIN(M,N+LL+1-J) - I1 + 1
               CALL F06FJF(NI,A(I1,J),1,SCALE,SUM)
   20       CONTINUE
            IF (Y90WAF(MATRIX,'S')) THEN
               SUM = TWO*SUM
            END IF
            CALL F06FJF(MIN(M,N),A(1,LL+1),1,SCALE,SUM)
C
         ELSE IF (Y90WAF(MATRIX,'U') .OR. Y90WAF(MATRIX,'Y')) THEN
            DO 40 J = 2, LU + 1
               NI = MIN(M,N+LL+1-J)
               CALL F06FJF(NI,A(1,J),1,SCALE,SUM)
   40       CONTINUE
            IF (Y90WAF(MATRIX,'Y')) THEN
               SUM = TWO*SUM
            END IF
            CALL F06FJF(MIN(M,N),A(1,1),1,SCALE,SUM)
C
         ELSE
            DO 60 J = 1, LL + LU + 1
               I1 = LL + 2 - J
               NI = MIN(M,N+LL+1-J) - I1 + 1
               CALL F06FJF(NI,A(I1,J),1,SCALE,SUM)
   60       CONTINUE
C
         END IF
         VALUE = F06BMF(SCALE,SUM)
C-----------------------------------------------------------------------
C
C     Calculate 1-Norm of A
C
C-----------------------------------------------------------------------
      ELSE IF (Y90WAF(NORM,'1')) THEN
C
         IF (Y90WAF(MATRIX,'L') .OR. Y90WAF(MATRIX,'S')) THEN
            DO 120 J = 1, MIN(N,M)
               SUM = ZERO
               DO 80 I = J, MIN(M,J+LL)
                  SUM = SUM + ABS(A(I,LL+J+1-I))
   80          CONTINUE
               IF (Y90WAF(MATRIX,'S')) THEN
                  DO 100 I = MAX(J-LL,1), J - 1
                     SUM = SUM + ABS(A(J,LL+I+1-J))
  100             CONTINUE
               END IF
               VALUE = MAX(VALUE,SUM)
  120       CONTINUE
C
         ELSE IF (Y90WAF(MATRIX,'U') .OR. Y90WAF(MATRIX,'Y')) THEN
            DO 180 J = 1, MIN(N,M+LU)
               SUM = ZERO
               DO 140 I = MAX(1,J-LU), J
                  SUM = SUM + ABS(A(I,J-I+1))
  140          CONTINUE
               IF (Y90WAF(MATRIX,'Y')) THEN
                  DO 160 I = J + 1, MIN(N,J+LU)
                     SUM = SUM + ABS(A(J,I+1-J))
  160             CONTINUE
               END IF
               VALUE = MAX(VALUE,SUM)
  180       CONTINUE
C
         ELSE
            DO 220 J = 1, N
               SUM = ZERO
               DO 200 I = MAX(1,J-LU), MIN(M,J+LL)
                  SUM = SUM + ABS(A(I,LL+J+1-I))
  200          CONTINUE
               VALUE = MAX(VALUE,SUM)
  220       CONTINUE
C
         END IF
C-----------------------------------------------------------------------
C
C     Calculate max(abs(A(i,j))
C
C-----------------------------------------------------------------------
      ELSE
         DO 260 J = 1, LL + LU + 1
            DO 240 I = MAX(1,LL+2-J), MIN(M,N+LL+1-J)
               VALUE = MAX(VALUE,ABS(A(I,J)))
  240       CONTINUE
  260    CONTINUE
      END IF
C
      Y90SDF = VALUE
C-----------------------------------------------------------------------
C
C     End of Y90SDF
C
C-----------------------------------------------------------------------
      RETURN
      END
