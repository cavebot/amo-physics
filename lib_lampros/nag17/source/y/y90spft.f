      SUBROUTINE Y90SPF(SELECT,M,N,IROW,L,D,A,IA,B,IB,WORK,IWORK)
C     MARK 15 RE-ISSUE. NAG COPYRIGHT 1991.
C-----------------------------------------------------------------------
C
C         ===========================================================
C         *  Y90SPF :  Product of a var.band with a generic matrix  *
C         ===========================================================
C
C-----------------------------------------------------------------------
C     .. Parameters ..
      DOUBLE PRECISION  ZERO
      PARAMETER         (ZERO=0.0D0)
C     .. Scalar Arguments ..
      INTEGER           IA, IB, IWORK, M, N, SELECT
C     .. Array Arguments ..
      DOUBLE PRECISION  A(IA,*), B(IB,*), D(*), L(*), WORK(IWORK,*)
      INTEGER           IROW(*)
C     .. Local Scalars ..
      DOUBLE PRECISION  TEMP
      INTEGER           I, J, K
C     .. External Subroutines ..
      EXTERNAL          F06EDF, Y90SHF
C     .. Executable Statements ..
C-----------------------------------------------------------------------
C
C     Carry out product
C
C-----------------------------------------------------------------------
      IF (SELECT.LE.6) THEN
C
C     Initialise
C
         CALL Y90SHF('N',M,N,A,IA,B,IB)
C
C     Multiply by upper triangular matrix L'
C
         IF (SELECT.NE.2 .AND. SELECT.NE.5) THEN
C
            DO 40 J = 1, N
               DO 20 I = 1, M
                  WORK(I,J) = ZERO
   20          CONTINUE
   40       CONTINUE
C
            DO 100 I = 1, M
               DO 80 K = I - IROW(I+1) + IROW(I) + 1, I
                  TEMP = L(IROW(I+1)-I+K-1)
                  DO 60 J = 1, N
                     WORK(K,J) = WORK(K,J) + TEMP*A(I,J)
   60             CONTINUE
   80          CONTINUE
  100       CONTINUE
            CALL Y90SHF('N',M,N,WORK,IWORK,B,IB)
C
         ELSE
            CALL Y90SHF('N',M,N,A,IA,B,IB)
C
         END IF
C
C     Multiply by diagonal matrix D
C
         IF (SELECT.LE.3) THEN
            DO 120 I = 1, M
               CALL F06EDF(N,D(I),B(I,1),IB)
  120       CONTINUE
         END IF
C
C     Multiply by lower triangular matrix L
C
         IF (SELECT.NE.3 .AND. SELECT.NE.6) THEN
C
            DO 160 J = 1, N
               DO 140 I = 1, M
                  WORK(I,J) = ZERO
  140          CONTINUE
  160       CONTINUE
C
            DO 220 I = 1, M
               DO 200 K = I - IROW(I+1) + IROW(I) + 1, I
                  TEMP = L(IROW(I+1)-I+K-1)
                  DO 180 J = 1, N
                     WORK(I,J) = WORK(I,J) + TEMP*B(K,J)
  180             CONTINUE
  200          CONTINUE
  220       CONTINUE
            CALL Y90SHF('N',M,N,WORK,IWORK,B,IB)
         END IF
C-----------------------------------------------------------------------
C
C     Product of symmetric matrix by generic matrix
C
C-----------------------------------------------------------------------
      ELSE
         DO 260 J = 1, N
            DO 240 I = 1, M
               B(I,J) = ZERO
  240       CONTINUE
  260    CONTINUE
C
         DO 320 I = 1, M
            DO 300 K = I - IROW(I+1) + IROW(I) + 1, I
               TEMP = L(IROW(I+1)-I+K-1)
               DO 280 J = 1, N
                  B(I,J) = B(I,J) + TEMP*A(K,J)
                  IF (K.NE.I) B(K,J) = B(K,J) + TEMP*A(I,J)
  280          CONTINUE
  300       CONTINUE
  320    CONTINUE
C
      END IF
C-----------------------------------------------------------------------
C
C     End of Y90SPF
C
C-----------------------------------------------------------------------
      RETURN
      END
