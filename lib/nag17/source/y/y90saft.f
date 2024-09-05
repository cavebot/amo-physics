      SUBROUTINE Y90SAF(MATRIX,M,N,KL,KU,A,IA)
C     MARK 15 RE-ISSUE. NAG COPYRIGHT 1991.
C-----------------------------------------------------------------------
C
C         ======================================================
C         *  Y90SAF :  Blank out Unused Part of Banded Matrix  *
C         ======================================================
C
C
C-----------------------------------------------------------------------
C     .. Parameters ..
      DOUBLE PRECISION  ZERO
      PARAMETER         (ZERO=0.0D0)
C     .. Scalar Arguments ..
      INTEGER           IA, KL, KU, M, N
      CHARACTER*1       MATRIX
C     .. Array Arguments ..
      DOUBLE PRECISION  A(IA,*)
C     .. Local Scalars ..
      INTEGER           I, J, LL, LU
C     .. External Functions ..
      LOGICAL           Y90WAF
      EXTERNAL          Y90WAF
C     .. Intrinsic Functions ..
      INTRINSIC         MAX, MIN
C     .. Executable Statements ..
C-----------------------------------------------------------------------
C
C     Execute
C
C-----------------------------------------------------------------------
      LL = KL
      LU = KU
      IF (Y90WAF(MATRIX,'L') .OR. Y90WAF(MATRIX,'S')) THEN
         LU = 0
      ELSE IF (Y90WAF(MATRIX,'U') .OR. Y90WAF(MATRIX,'Y')) THEN
         LL = 0
      END IF
C
      DO 60 J = 1, LL + LU + 1
         DO 20 I = 1, MAX(LL+1-J,0)
            A(I,J) = ZERO
   20    CONTINUE
         DO 40 I = MIN(M,N+LL+1-J) + 1, M
            A(I,J) = ZERO
   40    CONTINUE
   60 CONTINUE
C-----------------------------------------------------------------------
C
C     End of Y90SAF
C
C-----------------------------------------------------------------------
      RETURN
      END
