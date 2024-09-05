      SUBROUTINE F01QAY(N,X,NORM,Z1,SMALL,TINY,BIG)
C     MARK 8 RELEASE. NAG COPYRIGHT 1979.
C     MARK 11.5(F77) REVISED. (SEPT 1985.)
C     MARK 13 REVISED. USE OF MARK 12 X02 FUNCTIONS (APR 1988).
C     WRITTEN BY S. HAMMARLING, MIDDLESEX POLYTECHNIC (HOUSH0)
C
C     F01QAY FORMS DETAILS OF THE HOUSEHOLDER TRANSFORMATION
C
C     Y = (I-2*U*(U**T))*X ,  (U**T)*U = 1 ,
C
C     WHERE X IS AN N ELEMENT VECTOR AND THE N ELEMENT VECTOR
C     U IS CHOSEN SO THAT
C
C     Y(I) = 0 , I=2,3,...,N .
C
C     IN PLACE OF U THE ROUTINE ACTUALLY COMPUTES THE VECTOR Z
C     GIVEN BY
C
C     Z = (2*U(1))*U , SO THAT  2*U*(U**T) = (1/Z(1))*Z*(Z**T) .
C
C     THE ELEMENTS Z(2),Z(3),...,Z(N) ARE OVERWRITTEN ON THE
C     ELEMENTS X(2),X(3),...,X(N) RESPECTIVELY AND THE ELEMENT
C     Z(1) IS RETURNED IN Z1.
C
C     Y(1) IS OVERWRITTEN ON X(1).
C
C     N MUST BE AT LEAST 1. IF N=1 THEN AN IMMEDIATE RETURN TO
C     THE CALLING PROGRAM IS MADE.
C
C     NORM MUST BE .TRUE. IF THE EUCLIDEAN NORM OF X IS
C     SUPPLIED IN Z1 AND MUST BE .FALSE. OTHERWISE.
C
C     SMALL, TINY AND BIG MUST BE
C
C     SMALL = X02AMF ,  TINY = SQRT(X02AMF)   AND   BIG = 1.0/X02AMF
C
C     WHERE X02AMF IS THE SMALL VALUE RETURNED FROM ROUTINE X02AMF.
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION  BIG, SMALL, TINY, Z1
      INTEGER           N
      LOGICAL           NORM
C     .. Array Arguments ..
      DOUBLE PRECISION  X(N)
C     .. Local Scalars ..
      DOUBLE PRECISION  D, P, Q
      INTEGER           I
C     .. External Functions ..
      DOUBLE PRECISION  F04JGV
      EXTERNAL          F04JGV
C     .. Intrinsic Functions ..
      INTRINSIC         ABS
C     .. Executable Statements ..
      IF (N.EQ.1) RETURN
C
      IF (NORM) D = Z1
C
      IF ( .NOT. NORM) D = F04JGV(N,X,TINY,BIG)
C
      Z1 = 2.0D0
      IF (D.EQ.0.0D0) RETURN
C
      Q = 0.0D0
      IF (D.GT.1.0D0) Q = D*SMALL
      IF (X(1).LT.0.0D0) D = -D
C
      DO 20 I = 1, N
         P = 0.0D0
         IF (ABS(X(I)).GE.Q) P = X(I)/D
         X(I) = P
   20 CONTINUE
C
      Z1 = 1.0D0 + X(1)
      X(1) = -D
C
      RETURN
      END
