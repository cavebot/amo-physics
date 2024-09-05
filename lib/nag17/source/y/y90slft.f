      SUBROUTINE Y90SLF(MATRIX,PREC,N,M,A,IA,B,IB,X,IX,R,IR)
C     MARK 15 RE-ISSUE. NAG COPYRIGHT 1991.
C-----------------------------------------------------------------------
C
C         =====================================
C         *  Y90SLF :  Calculate the residual  *
C         =====================================
C
C-----------------------------------------------------------------------
C     .. Scalar Arguments ..
      INTEGER           IA, IB, IR, IX, M, N
      CHARACTER*1       MATRIX, PREC
C     .. Array Arguments ..
      DOUBLE PRECISION  A(IA,*), B(IB,*), R(IR,*), X(IX,*)
C     .. Local Scalars ..
      DOUBLE PRECISION  A1, A2, D11, D2
      INTEGER           I, IFAIL, J
      LOGICAL           SW
C     .. External Functions ..
      LOGICAL           Y90WAF
      EXTERNAL          Y90WAF
C     .. External Subroutines ..
      EXTERNAL          X03AAF
C     .. Executable Statements ..
C-----------------------------------------------------------------------
C
C     Calculate residual (using enhanced precision if so wished)
C
C-----------------------------------------------------------------------
      IF (Y90WAF(PREC,'D')) THEN
         SW = .TRUE.
      ELSE
         SW = .FALSE.
      END IF
C
      IFAIL = 1
C
C     Calculate residual (using enhanced precision if so wished)
C
C     Lower triangular matrix
C
      IF (Y90WAF(MATRIX,'L')) THEN
         DO 40 J = 1, M
            DO 20 I = 1, N
               CALL X03AAF(A(I,1),N*IA-I+1,X(1,J),(M-J+1)*IX,I,IA,1,
     *                     -B(I,J),0.0D0,D11,D2,SW,IFAIL)
               A1 = D11
               A2 = D2
               CALL X03AAF(A(I,I),(N-I+1)*IA-I+1,X(I,J),(IR-J+1)*IX-I+1,
     *                     N-I+1,1,1,A1,A2,D11,D2,SW,IFAIL)
               R(I,J) = -D11
   20       CONTINUE
   40    CONTINUE
C
C     Upper triangular matrix
C
      ELSE IF (Y90WAF(MATRIX,'U')) THEN
         DO 80 J = 1, M
            DO 60 I = 1, N
               CALL X03AAF(A(1,I),(N-I+1)*IA,X(1,J),(M-J+1)*IX,I-1,1,1,
     *                     -B(I,J),0.0D0,D11,D2,SW,IFAIL)
               A1 = D11
               A2 = D2
               CALL X03AAF(A(I,I),(N-I+1)*IA-I+1,X(I,J),(IR-J+1)*IX-I+1,
     *                     N-I+1,IA,1,A1,A2,D11,D2,SW,IFAIL)
               R(I,J) = -D11
   60       CONTINUE
   80    CONTINUE
C
C     Square matrix
C
      ELSE
         DO 120 J = 1, M
            DO 100 I = 1, N
               CALL X03AAF(A(I,1),N*IA-I+1,X(1,J),M*IX,N,IA,1,-B(I,J),
     *                     0.0D0,D11,D2,SW,IFAIL)
               R(I,J) = -D11
  100       CONTINUE
  120    CONTINUE
      END IF
C-----------------------------------------------------------------------
C
C     End of Y90SLF
C
C-----------------------------------------------------------------------
      RETURN
      END
