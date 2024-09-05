      SUBROUTINE Y90TCF(ORDER,V,N)
C     MARK 15 RE-ISSUE. NAG COPYRIGHT 1991.
C-----------------------------------------------------------------------
C
C         ========================================
C         *  Y90TCF Shell sort of a real vector  *
C         ========================================
C
C-----------------------------------------------------------------------
C     .. Scalar Arguments ..
      INTEGER           N
      CHARACTER*1       ORDER
C     .. Array Arguments ..
      DOUBLE PRECISION  V(*)
C     .. Local Scalars ..
      DOUBLE PRECISION  TEMP
      INTEGER           I, J, K, L, M
      LOGICAL           DONE
C     .. External Functions ..
      LOGICAL           Y90WAF
      EXTERNAL          Y90WAF
C     .. Intrinsic Functions ..
      INTRINSIC         INT, LOG, DBLE
C     .. Executable Statements ..
C-----------------------------------------------------------------------
C
C     Carry out shell sort
C
C-----------------------------------------------------------------------
      M = INT(LOG(DBLE(N))/LOG(2.0D0))
C
C     Sort in ascending order
C
      IF (Y90WAF(ORDER,'A')) THEN
         K = 2**M
         DO 80 I = M + 1, 1, -1
            DO 40 L = 1, N/K
               DONE = .TRUE.
               DO 20 J = 1, N - K
                  IF (V(J).GT.V(J+K)) THEN
                     TEMP = V(J)
                     V(J) = V(J+K)
                     V(J+K) = TEMP
                     DONE = .FALSE.
                  END IF
   20          CONTINUE
               IF (DONE) GO TO 60
   40       CONTINUE
   60       CONTINUE
            K = K/2
   80    CONTINUE
C
C     Sort in descending order
C
      ELSE
         K = 2**M
         DO 160 I = M + 1, 1, -1
            DO 120 L = 1, N/K
               DONE = .TRUE.
               DO 100 J = 1, N - K
                  IF (V(J).LT.V(J+K)) THEN
                     TEMP = V(J)
                     V(J) = V(J+K)
                     V(J+K) = TEMP
                     DONE = .FALSE.
                  END IF
  100          CONTINUE
               IF (DONE) GO TO 140
  120       CONTINUE
  140       CONTINUE
            K = K/2
  160    CONTINUE
      END IF
C-----------------------------------------------------------------------
C
C     End of Y90TCF
C
C-----------------------------------------------------------------------
      RETURN
      END
