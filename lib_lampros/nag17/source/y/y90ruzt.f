      SUBROUTINE Y90RUZ(N,DENS,NNZ,A,IROW,ICOL,IDIMA,SEED,WORK,IWORK)
C     MARK 17 RELEASE. NAG COPYRIGHT 1995.
*-----------------------------------------------------------------------
*
*         ====================================
*         *  Y90RUZ :  Auxiliary for Y90RUF  *
*         ====================================
*
*     Purpose
*     =======
*     Takes a real symmetric matrix in coordinate storage (CS) format,
*     and performs a sequence of orthogonal similarity transformations
*     using randomly indexed elementary plane rotations, until the
*     density of the matrix just exceeds DENS.
*
*-----------------------------------------------------------------------
C     .. Parameters ..
      DOUBLE PRECISION  HUND
      PARAMETER         (HUND=1.0D2)
C     .. Scalar Arguments ..
      DOUBLE PRECISION  DENS
      INTEGER           IDIMA, N, NNZ
C     .. Array Arguments ..
      DOUBLE PRECISION  A(IDIMA), WORK(2*N)
      INTEGER           ICOL(IDIMA), IROW(IDIMA), IWORK(3*N), SEED(4)
C     .. Local Scalars ..
      DOUBLE PRECISION  C, D, S, T
      INTEGER           I, IERYCF, IWC, IWR, J, NEXT, P, Q
      CHARACTER         DUP, RMZERO
C     .. External Functions ..
      DOUBLE PRECISION  Y90TBF
      EXTERNAL          Y90TBF
C     .. External Subroutines ..
      EXTERNAL          F11JAX, F11ZBF, Y90RUY
C     .. Intrinsic Functions ..
      INTRINSIC         DBLE, INT, SQRT
C     .. Executable Statements ..
*
*     Split workspace
*
      IWR = N + 1
      IWC = 2*N + 1
*
*     Convert to linked-list format
*
      CALL F11JAX(NNZ,N,IROW,IWORK(IWR),IWORK(1))
      CALL F11JAX(NNZ,N,ICOL,IWORK(IWC),IWORK(1))
*
*     Construct orthogonal similarity transformations using elementary
*     plane rotations.
*
*     Calculate density.
*
   20 CONTINUE
      D = HUND*DBLE(2*NNZ-N)/DBLE(N*N)
      IF (D.LT.DENS) THEN
*
*         Choose rotation plane.
*
         P = INT(N*Y90TBF(1,SEED)) + 1
   40    CONTINUE
         Q = INT(N*Y90TBF(1,SEED)) + 1
         IF (Q.EQ.P) GO TO 40
*
         C = Y90TBF(1,SEED)
         S = Y90TBF(2,SEED)
         T = SQRT(C*C+S*S)
         C = C/T
         S = S/T
*
         CALL Y90RUY(N,NNZ,P,Q,C,S,A,IROW,ICOL,IDIMA,IWORK(IWR),
     *               IWORK(IWC),WORK(1),WORK(N+1))
*
         GO TO 20
*
      END IF
*
*     Convert linked-list representation back to CS.
*
      DO 80 I = 1, N
         J = IWORK(IWR+I-1)
   60    CONTINUE
         NEXT = IROW(J)
         IROW(J) = I
         J = NEXT
         IF (NEXT.GT.0) GO TO 60
   80 CONTINUE
*
      DO 120 I = 1, N
         J = IWORK(IWC+I-1)
  100    CONTINUE
         NEXT = ICOL(J)
         ICOL(J) = I
         J = NEXT
         IF (NEXT.GT.0) GO TO 100
  120 CONTINUE
*
*     Sort non-zero elements into row order, with column order within
*     each row.
*
      DUP = 'R'
      RMZERO = 'R'
      IERYCF = 0
      CALL F11ZBF(N,NNZ,A,IROW,ICOL,DUP,RMZERO,IWORK(IWR),IWORK(1),
     *            IERYCF)
*
*     End of subroutine Y90RUZ
*
      RETURN
      END
