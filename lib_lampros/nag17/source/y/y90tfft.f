      SUBROUTINE Y90TFF(N,RR,RI,IORD1,IORD2)
C     MARK 15 RE-ISSUE. NAG COPYRIGHT 1991.
C-----------------------------------------------------------------------
C
C         ======================================
C         *  Y90TFF Eigenvalues Shell Sorting  *
C         ======================================
C
C-----------------------------------------------------------------------
C     .. Parameters ..
      DOUBLE PRECISION  TWO
      PARAMETER         (TWO=2.0D0)
C     .. Scalar Arguments ..
      INTEGER           N
C     .. Array Arguments ..
      DOUBLE PRECISION  RI(*), RR(*)
      INTEGER           IORD1(*), IORD2(*)
C     .. Local Scalars ..
      DOUBLE PRECISION  TEMP1, TEMP2
      INTEGER           I, ITEMP, J, K, L, M
      LOGICAL           DONE
C     .. Intrinsic Functions ..
      INTRINSIC         INT, LOG, DBLE, SQRT
C     .. Executable Statements ..
C-----------------------------------------------------------------------
C
C     Carry out shell sort
C
C-----------------------------------------------------------------------
      M = INT(LOG(DBLE(N))/LOG(TWO))
      DO 20 I = 1, N
         IORD1(I) = I
   20 CONTINUE
C
C     Sort in descending order
C
      K = 2**M
      DO 100 I = M + 1, 1, -1
         DO 60 L = 1, N/K
            DONE = .TRUE.
            DO 40 J = 1, N - K
               TEMP1 = SQRT(RR(IORD1(J))**2+RI(IORD1(J))**2)
               TEMP2 = SQRT(RR(IORD1(J+K))**2+RI(IORD1(J+K))**2)
               IF (TEMP1.LT.TEMP2) THEN
                  ITEMP = IORD1(J)
                  IORD1(J) = IORD1(J+K)
                  IORD1(J+K) = ITEMP
                  DONE = .FALSE.
               END IF
   40       CONTINUE
            IF (DONE) GO TO 80
   60    CONTINUE
   80    CONTINUE
         K = K/2
  100 CONTINUE
C
C     Complete
C
      DO 120 I = 1, N
         IORD2(IORD1(I)) = I
  120 CONTINUE
C-----------------------------------------------------------------------
C
C     End of Y90TFF
C
C-----------------------------------------------------------------------
      RETURN
      END
