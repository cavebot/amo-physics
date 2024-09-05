      SUBROUTINE Y90SKF(MATRIX,A,NDA,N)
C     MARK 15 RE-ISSUE. NAG COPYRIGHT 1991.
C-----------------------------------------------------------------------
C
C         ==========================================
C         *  Y90SKF :  Symmetrize a square matrix  *
C         ==========================================
C
C-----------------------------------------------------------------------
C     .. Parameters ..
      DOUBLE PRECISION  HALF
      PARAMETER         (HALF=0.5D0)
C     .. Scalar Arguments ..
      INTEGER           N, NDA
      CHARACTER*1       MATRIX
C     .. Array Arguments ..
      DOUBLE PRECISION  A(NDA,*)
C     .. Local Scalars ..
      DOUBLE PRECISION  Z
      INTEGER           I, J
C     .. External Functions ..
      LOGICAL           Y90WAF
      EXTERNAL          Y90WAF
C     .. Executable Statements ..
C-----------------------------------------------------------------------
C
C     Symmetrize the matrix
C
C-----------------------------------------------------------------------
C
C     Copy upper triangular part into lower triangular part
C
      IF (Y90WAF(MATRIX,'U')) THEN
         DO 40 J = 1, N
            DO 20 I = J + 1, N
               A(I,J) = A(J,I)
   20       CONTINUE
   40    CONTINUE
C
C     Copy lower triangular part into upper triangular part
C
      ELSE IF (Y90WAF(MATRIX,'L')) THEN
         DO 80 J = 1, N
            DO 60 I = J + 1, N
               A(J,I) = A(I,J)
   60       CONTINUE
   80    CONTINUE
C
C     Average symmetric elements
C
      ELSE
         DO 120 J = 1, N
            DO 100 I = J + 1, N
               Z = HALF*(A(I,J)+A(J,I))
               A(I,J) = Z
               A(J,I) = Z
  100       CONTINUE
  120    CONTINUE
      END IF
C-----------------------------------------------------------------------
C
C     End of Y90SKF
C
C-----------------------------------------------------------------------
      RETURN
      END
