      SUBROUTINE Y90TEF(N,K,L,D,INTGER,A,IA,W1,IW1,W2,IW2)
C     MARK 15 RE-ISSUE. NAG COPYRIGHT 1991.
C-----------------------------------------------------------------------
C
C         ===========================================
C         *  Y90TEF Obtain Hessenberg eigenvectors  *
C         ===========================================
C
C-----------------------------------------------------------------------
C     .. Parameters ..
      DOUBLE PRECISION  ZERO, ONE
      PARAMETER         (ZERO=0.0D0,ONE=1.0D0)
C     .. Scalar Arguments ..
      INTEGER           IA, IW1, IW2, K, L, N
C     .. Array Arguments ..
      DOUBLE PRECISION  A(IA,*), D(*), W1(IW1,*), W2(IW2,*)
      INTEGER           INTGER(*)
C     .. Local Scalars ..
      INTEGER           I, J, LK, M
C     .. External Subroutines ..
      EXTERNAL          F06ECF, F06EDF, F06EGF, F06QHF, Y90SHF
C     .. Intrinsic Functions ..
      INTRINSIC         NINT
C     .. Executable Statements ..
C-----------------------------------------------------------------------
C
C     1. Invert the permutation & balancing
C
C-----------------------------------------------------------------------
      LK = L - K + 1
      CALL F06QHF('G',N,N,ZERO,ONE,W1,IW1)
C
      DO 20 I = N, L + 1, -1
         M = NINT(D(I))
         IF (M.NE.I) CALL F06EGF(N,W1(I,1),IW1,W1(M,1),IW1)
   20 CONTINUE
C
      DO 40 I = 1, K - 1
         M = NINT(D(I))
         IF (M.NE.I) CALL F06EGF(N,W1(I,1),IW1,W1(M,1),IW1)
   40 CONTINUE
C
      DO 60 I = K, L
         CALL F06EDF(N,ONE/D(I),W1(I,1),IW1)
   60 CONTINUE
C-----------------------------------------------------------------------
C
C     2. Invert the reduction to Hessenberg form
C
C-----------------------------------------------------------------------
      CALL Y90SHF('N',N,N,A,IA,W2,IW2)
      DO 80 I = L, K + 1, -1
         M = INTGER(I)
         IF (M.NE.I) CALL F06EGF(I-K-1,W2(I,K),IW2,W2(M,K),IW2)
   80 CONTINUE
C
      DO 120 J = K, L - 2
         M = INTGER(J+1)
         IF (M.NE.J+1) CALL F06EGF(N,W1(J+1,1),IW1,W1(M,1),IW1)
         DO 100 I = J + 2, L
            CALL F06ECF(N,-W2(I,J),W1(J+1,1),IW1,W1(I,1),IW1)
  100    CONTINUE
  120 CONTINUE
C-----------------------------------------------------------------------
C
C     End of Y90TEF
C
C-----------------------------------------------------------------------
      RETURN
      END
