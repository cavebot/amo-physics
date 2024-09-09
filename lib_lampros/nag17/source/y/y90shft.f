      SUBROUTINE Y90SHF(MATRIX,M,N,A,NDA,B,NDB)
C     MARK 15 RE-ISSUE. NAG COPYRIGHT 1991.
C-----------------------------------------------------------------------
C
C         ==============================================
C         *  Y90SHF :  Copy a Matrix or its Transpose  *
C         ==============================================
C
C-----------------------------------------------------------------------
C     .. Scalar Arguments ..
      INTEGER           M, N, NDA, NDB
      CHARACTER*1       MATRIX
C     .. Array Arguments ..
      DOUBLE PRECISION  A(NDA,*), B(NDB,*)
C     .. Local Scalars ..
      INTEGER           I, J
C     .. External Functions ..
      LOGICAL           Y90WAF
      EXTERNAL          Y90WAF
C     .. Executable Statements ..
C-----------------------------------------------------------------------
C
C     Copy the Matrix Transpose
C
C-----------------------------------------------------------------------
      IF (Y90WAF(MATRIX,'T')) THEN
         DO 40 I = 1, N
            DO 20 J = 1, M
               B(I,J) = A(J,I)
   20       CONTINUE
   40    CONTINUE
C-----------------------------------------------------------------------
C
C     Copy the Matrix
C
C-----------------------------------------------------------------------
      ELSE
         DO 80 I = 1, N
            DO 60 J = 1, M
               B(J,I) = A(J,I)
   60       CONTINUE
   80    CONTINUE
      END IF
C-----------------------------------------------------------------------
C
C     End of Y90SHF
C
C-----------------------------------------------------------------------
      RETURN
      END
