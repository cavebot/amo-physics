      SUBROUTINE F02WBX(M,N,A,NRA,WANTB,B,SV,WORK,LWORK,IFAIL)
C     MARK 17 RELEASE. NAG COPYRIGHT 1995.
C     Originally called F02WBF
C     WRITTEN BY S. HAMMARLING, MIDDLESEX POLYTECHNIC (SVDGN2)
C
C     F02WBZ RETURNS PART OF THE SINGULAR VALUE DECOMPOSITION
C     OF THE M*N (M.LE.N) MATRIX A GIVEN BY
C
C     A = Q*(D 0)*(P**T) ,
C
C     WHERE Q AND P ARE ORTHOGONAL MATRICES AND D IS AN M*M
C     DIAGONAL MATRIX WITH NON-NEGATIVE DIAGONAL ELEMENTS,
C     THESE BEING THE SINGULAR VALUES OF A.
C
C     THE DIAGONAL ELEMENTS OF D AND THE FIRST M ROWS OF P**T
C     ARE RETURNED. IF WANTB IS .TRUE. THEN (Q**T)*B IS ALSO
C     RETURNED.
C
C     INPUT PARAMETERS.
C
C     M     - NUMBER OF ROWS OF A. M MUST BE AT LEAST UNITY.
C
C     N     - NUMBER OF COLUMNS OF A. N MUST BE AT LEAST M.
C
C     A     - THE M*N MATRIX TO BE FACTORIZED.
C
C     NRA   - ROW DIMENSION OF A AS DECLARED IN THE CALLING PROGRAM
C             NRA MUST BE AT LEAST M.
C
C     WANTB - MUST BE .TRUE. IF (Q**T)*B IS REQUIRED.
C             IF WANTB IS .FALSE. THEN B IS NOT REFERENCED.
C
C     B     - AN M ELEMENT VECTOR.
C
C     IFAIL - THE USUAL FAILURE PARAMETER. IF IN DOUBT SET
C             IFAIL TO ZERO BEFORE CALLING F02WBZ.
C
C     OUTPUT PARAMETERS.
C
C     A     - A WILL CONTAIN THE FIRST M ROWS OF P**T.
C
C     B     - IF WANTB IS .TRUE. THEN B IS OVERWRITTEN BY
C             THE M ELEMENT VECTOR (Q**T)*B.
C
C     SV    - M ELEMENT VECTOR CONTAINING THE SINGULAR
C             VALUES OF A. THEY ARE ORDERED SO THAT
C             SV(1).GE.SV(2).GE. ... .GE.SV(M).GE.0.
C
C     IFAIL - ON NORMAL RETURN IFAIL WILL BE ZERO.
C             IN THE UNLIKELY EVENT THAT THE QR-ALGORITHM
C             FAILS TO FIND THE SINGULAR VALUES IN 50*M
C             ITERATIONS THEN IFAIL IS SET TO 2.
C             IF AN INPUT PARAMETER IS INCORRECTLY SUPPLIED
C             THEN IFAIL IS SET TO UNITY.
C
C     WORKSPACE PARAMETERS.
C
C     WORK  - AN (M*M+3*M) ELEMENT VECTOR.
C             WORK(1) RETURNS THE TOTAL NUMBER OF ITERATIONS TAKEN
C             BY THE QR-ALGORITHM.
C
C     LWORK - THE LENGTH OF THE VECTOR WORK.
C             LWORK MUST BE AT LEAST M*M+3*M.
C
C     .. Parameters ..
      CHARACTER*6       SRNAME
      PARAMETER         (SRNAME='F02WBZ')
C     .. Scalar Arguments ..
      INTEGER           IFAIL, LWORK, M, N, NRA
      LOGICAL           WANTB
C     .. Array Arguments ..
      DOUBLE PRECISION  A(NRA,N), B(M), SV(M), WORK(LWORK)
C     .. Local Scalars ..
      INTEGER           IERR, J, K1, K2, K3
C     .. Local Arrays ..
      CHARACTER*1       P01REC(1)
C     .. External Functions ..
      INTEGER           P01ABF
      EXTERNAL          P01ABF
C     .. External Subroutines ..
      EXTERNAL          F01LZF, F01QBF, F02SZF, F02WAY, F02WBY, F02WBZ
C     .. Executable Statements ..
      IERR = IFAIL
      IF (IERR.EQ.0) IFAIL = 1
C
      IF (NRA.LT.M .OR. N.LT.M .OR. LWORK.LT.M*(M+3) .OR. M.LT.1)
     *    GO TO 40
C
      K1 = M + 1
      K2 = K1 + M
      K3 = K2 + M
C
      CALL F01QBF(M,N,A,NRA,A,NRA,WORK,IFAIL)
C
      CALL F01LZF(M,A,NRA,WORK(K3),M,WANTB,B,.FALSE.,.FALSE.,WORK,1,1,
     *            .FALSE.,WORK,1,1,SV,WORK,WORK,WORK,IFAIL)
C
      CALL F02WAY(M,WORK(K3),M,WORK(K3),M)
C
      IFAIL = 1
      CALL F02SZF(M,SV,WORK,SV,WANTB,B,.FALSE.,WORK,1,1,.TRUE.,WORK(K3)
     *            ,M,M,WORK,WORK(K1),WORK(K2),IFAIL)
C
      CALL F02WBZ(M,N,A,NRA,A,NRA,WORK(K1))
C
      DO 20 J = 1, N
C
         CALL F02WBY(M,M,WORK(K3),M,A(1,J),A(1,J),WORK(K1))
C
   20 CONTINUE
C
      IF (IFAIL.EQ.0) RETURN
C
      IFAIL = 2
   40 IFAIL = P01ABF(IERR,IFAIL,SRNAME,0,P01REC)
      RETURN
      END
