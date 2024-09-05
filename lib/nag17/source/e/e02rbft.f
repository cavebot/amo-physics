      SUBROUTINE E02RBF(A,IA,B,IB,X,ANS,IFAIL)
C     MARK 7 RELEASE. NAG COPYRIGHT 1978.
C     MARK 11.5(F77) REVISED. (SEPT 1985.)
C     MARK 13 REVISED. USE OF MARK 12 X02 FUNCTIONS (APR 1988).
C
C     E02RBF EVALUATES AN (L/M) PADE APPROXIMANT, WHOSE
C     NUMERATOR AND DENOMINATOR COEFFICIENTS ARE STORED IN
C     ARRAYS A AND B RESPECTIVELY, AT A GIVEN POINT X.
C
C     ARGUMENT LIST
C     -------------
C
C     A,B  (REAL ARRAYS) ON ENTRY CONTAINS THE NUMERATOR
C                        (DENOMINATOR) COEFFICIENTS OF THE PADE
C                        APPROXIMANT. UNCHANGED ON EXIT.
C     IA,IB  (INTEGERS)  ON ENTRY DEFINE IMPLICITLY THE ORDERS
C                        OF THE NUMERATOR IA=L+1, AND DENOMINATOR
C                        IB=M+1. UNCHANGED ON EXIT.
C     X        (REAL)    ON ENTRY DEFINES THE POINT OF EVALUATION
C                        OF THE PADE APPROXIMANT. UNCHANGED ON
C                        EXIT.
C     ANS      (REAL)    ON EXIT CONTAINS THE VALUE OF THE PADE
C                        APPROXIMANT AT X.
C     IFAIL   (INTEGER)  USED FOR ERROR EXITS.
C
C     .. Parameters ..
      CHARACTER*6       SRNAME
      PARAMETER         (SRNAME='E02RBF')
C     .. Scalar Arguments ..
      DOUBLE PRECISION  ANS, X
      INTEGER           IA, IB, IFAIL
C     .. Array Arguments ..
      DOUBLE PRECISION  A(IA), B(IB)
C     .. Local Scalars ..
      DOUBLE PRECISION  AERR, D, EPS, XABS
      INTEGER           I, I1, L, M
C     .. Local Arrays ..
      CHARACTER*1       P01REC(1)
C     .. External Functions ..
      DOUBLE PRECISION  X02AJF
      INTEGER           P01ABF
      EXTERNAL          X02AJF, P01ABF
C     .. Intrinsic Functions ..
      INTRINSIC         ABS
C     .. Executable Statements ..
      EPS = X02AJF()
C     TEST FOR SENSIBLE PARAMETERS IA AND IB  (IFAIL=2)
      IF (IA.LT.1 .OR. IB.LT.1) GO TO 160
      DO 20 I = 1, IB
         IF (B(I).NE.0.0D0) GO TO 40
   20 CONTINUE
      GO TO 160
C     EVALUATE DENOMINATOR
   40 D = B(IB)
      AERR = ABS(D*0.5D0)
      IF (IB.EQ.1) GO TO 80
      M = IB - 1
      XABS = ABS(X)
      DO 60 I = 1, M
         I1 = IB - I
         D = D*X + B(I1)
         AERR = XABS*AERR + ABS(D)
   60 CONTINUE
      AERR = EPS*(2.0D0*AERR-ABS(D))
C     TEST FOR SMALL DIVISOR
      IF (ABS(D).LE.AERR) GO TO 140
C     NUMERATOR EVALUATION
   80 ANS = A(IA)
      IF (IA.EQ.1) GO TO 120
      L = IA - 1
      DO 100 I = 1, L
         I1 = IA - I
         ANS = ANS*X + A(I1)
  100 CONTINUE
  120 ANS = ANS/D
      IFAIL = 0
      RETURN
C     EXIT FOR SMALL DIVISOR
  140 IFAIL = P01ABF(IFAIL,1,SRNAME,0,P01REC)
      RETURN
C     EXIT FOR WRONG PARAMETERS
  160 IFAIL = P01ABF(IFAIL,2,SRNAME,0,P01REC)
      RETURN
      END
