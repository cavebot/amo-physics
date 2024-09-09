      SUBROUTINE Y90TDF(N,K,L,D,INTGER,A,IA,W,IW)
C     MARK 15 RE-ISSUE. NAG COPYRIGHT 1991.
C-----------------------------------------------------------------------
C
C         ========================================
C         *  Y90TDF :  Obtain Full Eigenvectors  *
C         ========================================
C
C-----------------------------------------------------------------------
C     .. Scalar Arguments ..
      INTEGER           IA, IW, K, L, N
C     .. Array Arguments ..
      DOUBLE PRECISION  A(IA,*), D(*), W(IW,*)
      INTEGER           INTGER(*)
C     .. Local Scalars ..
      INTEGER           I, M
C     .. External Subroutines ..
      EXTERNAL          F01APF, F06EDF, F06EGF
C     .. Intrinsic Functions ..
      INTRINSIC         NINT
C     .. Executable Statements ..
C-----------------------------------------------------------------------
C
C     1. Carry out the reduction to Hessenberg form
C
C-----------------------------------------------------------------------
      CALL F01APF(N,K,L,INTGER,A,IA,W,IW)
C-----------------------------------------------------------------------
C
C     2. Carry out the permutation & balancing transformation
C
C-----------------------------------------------------------------------
      DO 20 I = K, L
         CALL F06EDF(N,D(I),W(I,1),IW)
   20 CONTINUE
C
      DO 40 I = K - 1, 1, -1
         M = NINT(D(I))
         IF (M.NE.I) CALL F06EGF(N,W(I,1),IW,W(M,1),IW)
   40 CONTINUE
C
      DO 60 I = L + 1, N
         M = NINT(D(I))
         IF (M.NE.I) CALL F06EGF(N,W(I,1),IW,W(M,1),IW)
   60 CONTINUE
C-----------------------------------------------------------------------
C
C     End of Y90TDF
C
C-----------------------------------------------------------------------
      RETURN
      END
