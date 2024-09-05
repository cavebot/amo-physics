      SUBROUTINE G02CEF(NVARS,XBAR,STD,SSP,ISSP,R,IR,NVARS2,KORDER,
     *                  XBAR2,STD2,SSP2,ISSP2,R2,IR2,IFAIL)
C     MARK 4 RELEASE NAG COPYRIGHT 1974.
C     MARK 4.5 REVISED
C     MARK 11.5(F77) REVISED. (SEPT 1985.)
C
C     NAG SUBROUTINE G02CEF
C     WRITTEN  6.10.73 BY PAUL GRIFFITHS (OXFORD UNIVERSITY)
C
C     TAKES SELECTED ELEMENTS FROM TWO VECTORS (TYPICALLY VECTORS
C     OF
C     MEANS AND STANDARD DEVIATIONS) TO FORM TWO SMALLER VECTORS,
C     AND SELECTED ROWS AND COLUMNS FROM TWO MATRICES (TYPICALLY
C     EITHER MATRICES OF SUMS OF SQUARES AND CROSS-PRODUCTS OF
C     DEVIATIONS FROM MEANS AND PEARSON PRODUCT-MOMENT CORRELATION
C     COEFFICIENTS, OR MATRICES OF SUMS OF SQUARES AND
C     CROSS-PRODUCTS
C     ABOUT ZERO AND CORRELATION-LIKE COEFFICIENTS) TO FORM TWO
C     SMALLER MATRICES, ALLOWING RE-ORDERING OF ELEMENTS IN THE
C     PROCESS.
C
C     USES NAG ERROR ROUTINE P01AAF
C
C
C     ABOVE DATA STATEMENT MAY BE MACHINE-DEPENDENT -- DEPENDS ON
C     NUMBER OF CHARACTERS WHICH CAN BE STORED IN A REAL VARIABLE
C
C     .. Parameters ..
      CHARACTER*6       SRNAME
      PARAMETER         (SRNAME='G02CEF')
C     .. Scalar Arguments ..
      INTEGER           IFAIL, IR, IR2, ISSP, ISSP2, NVARS, NVARS2
C     .. Array Arguments ..
      DOUBLE PRECISION  R(IR,NVARS), R2(IR2,NVARS2), SSP(ISSP,NVARS),
     *                  SSP2(ISSP2,NVARS2), STD(NVARS), STD2(NVARS2),
     *                  XBAR(NVARS), XBAR2(NVARS2)
      INTEGER           KORDER(NVARS2)
C     .. Local Scalars ..
      INTEGER           I, IERROR, J, K, L
C     .. Local Arrays ..
      CHARACTER*1       P01REC(1)
C     .. External Functions ..
      INTEGER           P01ABF
      EXTERNAL          P01ABF
C     .. Executable Statements ..
      IERROR = 0
      IF (ISSP.LT.NVARS .OR. IR.LT.NVARS .OR. ISSP2.LT.NVARS2 .OR.
     *    IR2.LT.NVARS2) IERROR = 3
      IF (NVARS.LT.NVARS2) IERROR = 2
      IF (NVARS.LT.2 .OR. NVARS2.LT.1) IERROR = 1
      IF (IERROR) 20, 20, 120
   20 DO 40 I = 1, NVARS2
         IF (KORDER(I).LT.1 .OR. KORDER(I).GT.NVARS) GO TO 100
   40 CONTINUE
      DO 80 I = 1, NVARS2
         J = KORDER(I)
         XBAR2(I) = XBAR(J)
         STD2(I) = STD(J)
         DO 60 K = 1, I
            L = KORDER(K)
            SSP2(I,K) = SSP(J,L)
            SSP2(K,I) = SSP(L,J)
            R2(I,K) = R(J,L)
            R2(K,I) = R(L,J)
   60    CONTINUE
   80 CONTINUE
      IFAIL = 0
      RETURN
  100 IERROR = 4
  120 IFAIL = P01ABF(IFAIL,IERROR,SRNAME,0,P01REC)
      RETURN
      END
