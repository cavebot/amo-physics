      SUBROUTINE Y90SGF(TYPE,NLOSE,A,LDA,M,N)
C     MARK 15 RE-ISSUE. NAG COPYRIGHT 1991.
C-----------------------------------------------------------------------
C
C         ==========================================================
C         *  Y90SGF :  Chops off binary digits from a real matrix  *
C         ==========================================================
C
C
C     This routines rounds off the least significant bits of the
C     elements of a real matrix.  The number of bits to be lost is
C     specified in input by the parameter NLOSE.  The whole matrix
C     or each column or row individually can be rounded off (see the
C     input parameter TYPE below).  The largest element in each column /
C     row / the whole matrix is detected and is rounded off to lose
C     NLOSE bits.  The remaining elements in the column / row / whole
C     matrix are then rounded to the SAME LEVEL as the largest element
C     to ensure that all elements in a column / row / the whole matrix
C     have least significant bits of the same order of magnitude in an
C     absolute scale.
C
C
C     Argument List
C     -------------
C
C     NLOSE     Integer, Input.
C               Number of bits to be rounded off with respect to the
C               largest element in each row / column / the whole matrix
C               (according to the value in TYPE).
C
C     A(LDA,*)  Double precision, Input/Output.
C               The matrix to be rouded off.
C
C     LDA       Integer, Input.
C               Leading dimension of the array A.
C
C     TYPE      Character*1, Input.
C               Defines how the lopping off of the trailing bits is
C               to be carries out.
C               'C'  ==>  Each column individually.
C               'R'  ==>  Each row individually.
C               'A'  ==>  The whole matrix.
C
C     M         Integer, Input.
C               Number of rows of the matrix A.
C
C     N         Integer, Input.
C               Number of columns of the matrix A.
C
C
C-----------------------------------------------------------------------
C     .. Parameters ..
      DOUBLE PRECISION  ZERO, ONE
      PARAMETER         (ZERO=0.0D0,ONE=1.0D0)
C     .. Scalar Arguments ..
      INTEGER           LDA, M, N, NLOSE
      CHARACTER*1       TYPE
C     .. Array Arguments ..
      DOUBLE PRECISION  A(LDA,*)
C     .. Local Scalars ..
      DOUBLE PRECISION  AMAX, BASE, FACT, X, Y, Z
      INTEGER           I, J, JEXP, K, L
C     .. External Functions ..
      INTEGER           F06JLF
      LOGICAL           Y90WAF
      EXTERNAL          F06JLF, Y90WAF
C     .. Intrinsic Functions ..
      INTRINSIC         ABS, INT, LOG, MAX, MOD, SIGN
C     .. Statement Functions ..
      DOUBLE PRECISION  FUNC1, FUNC2
C     .. Statement Function definitions ..
      FUNC1(X,Y) = Y + SIGN(X,Y)
      FUNC2(X,Y) = Y + SIGN(X,-Y)
C     .. Executable Statements ..
C-----------------------------------------------------------------------
C
C     Initialize
C
C-----------------------------------------------------------------------
      BASE = 2
C-----------------------------------------------------------------------
C
C     Chop each column of the matrix
C
C-----------------------------------------------------------------------
      IF (Y90WAF(TYPE,'C')) THEN
C
         DO 100 J = 1, N
C
C     Calculate the scaling
C
            K = F06JLF(M,A(1,J),1)
            AMAX = ABS(A(K,J))
C
            IF (AMAX.NE.ZERO) THEN
C
               JEXP = INT(LOG(AMAX)/LOG(BASE))
               IF (JEXP.LE.0) JEXP = JEXP - 1
               K = JEXP + NLOSE
               IF (K.GE.0) THEN
                  Z = BASE
               ELSE
                  K = -K
                  Z = ONE/BASE
               END IF
C
               FACT = 1
               DO 20 L = 1, 20
                  IF (K.LE.0) GO TO 40
                  IF (MOD(K,2).GT.0) FACT = FACT*Z
                  Z = Z*Z
                  K = K/2
   20          CONTINUE
   40          CONTINUE
C
C     Now scale the column
C
               IF (NLOSE.GT.0) THEN
                  DO 60 I = 1, M
                     A(I,J) = FUNC1(FACT,A(I,J))
                     A(I,J) = FUNC2(FACT,A(I,J))
   60             CONTINUE
               ELSE
                  DO 80 I = 1, M
                     IF (AMAX/ABS(A(I,J)).GT.2) THEN
                        A(I,J) = FUNC1(FACT,A(I,J))
                        A(I,J) = FUNC2(FACT,A(I,J))
                     END IF
   80             CONTINUE
               END IF
C
            END IF
C
  100    CONTINUE
C-----------------------------------------------------------------------
C
C     Chop each row of the matrix
C
C-----------------------------------------------------------------------
      ELSE IF (Y90WAF(TYPE,'R')) THEN
C
         DO 200 I = 1, M
C
C     Calculate the scaling
C
            K = F06JLF(N,A(I,1),LDA)
            AMAX = ABS(A(I,K))
C
            IF (AMAX.NE.ZERO) THEN
C
               JEXP = INT(LOG(AMAX)/LOG(BASE))
               IF (JEXP.LE.0) JEXP = JEXP - 1
               K = JEXP + NLOSE
               IF (K.GE.0) THEN
                  Z = BASE
               ELSE
                  K = -K
                  Z = ONE/BASE
               END IF
C
               FACT = 1
               DO 120 L = 1, 20
                  IF (K.LE.0) GO TO 140
                  IF (MOD(K,2).GT.0) FACT = FACT*Z
                  Z = Z*Z
                  K = K/2
  120          CONTINUE
  140          CONTINUE
C
C     Now scale the row
C
               IF (NLOSE.GT.0) THEN
                  DO 160 J = 1, N
                     A(I,J) = FUNC1(FACT,A(I,J))
                     A(I,J) = FUNC2(FACT,A(I,J))
  160             CONTINUE
               ELSE
                  DO 180 J = 1, N
                     IF (AMAX/ABS(A(I,J)).GT.2) THEN
                        A(I,J) = FUNC1(FACT,A(I,J))
                        A(I,J) = FUNC2(FACT,A(I,J))
                     END IF
  180             CONTINUE
               END IF
C
            END IF
C
  200    CONTINUE
C-----------------------------------------------------------------------
C
C     Chop the whole matrix
C
C-----------------------------------------------------------------------
      ELSE
C
C     Calculate the scaling
C
         AMAX = ZERO
         DO 220 J = 1, N
            K = F06JLF(M,A(1,J),1)
            AMAX = MAX(ABS(A(K,J)),AMAX)
  220    CONTINUE
C
         IF (AMAX.NE.ZERO) THEN
C
            JEXP = INT(LOG(AMAX)/LOG(BASE))
            IF (JEXP.LE.0) JEXP = JEXP - 1
            K = JEXP + NLOSE
            IF (K.GE.0) THEN
               Z = BASE
            ELSE
               K = -K
               Z = ONE/BASE
            END IF
C
            FACT = 1
            DO 240 L = 1, 20
               IF (K.LE.0) GO TO 260
               IF (MOD(K,2).GT.0) FACT = FACT*Z
               Z = Z*Z
               K = K/2
  240       CONTINUE
  260       CONTINUE
C
C     Now scale the matrix
C
            IF (NLOSE.GT.0) THEN
               DO 300 J = 1, N
                  DO 280 I = 1, M
                     X = FUNC1(FACT,A(I,J))
                     A(I,J) = FUNC2(FACT,X)
  280             CONTINUE
  300          CONTINUE
            ELSE
               DO 340 J = 1, N
                  DO 320 I = 1, M
                     IF (AMAX/ABS(A(I,J)).GT.2) THEN
                        X = FUNC1(FACT,A(I,J))
                        A(I,J) = FUNC2(FACT,X)
                     END IF
  320             CONTINUE
  340          CONTINUE
            END IF
C
         END IF
C
      END IF
C-----------------------------------------------------------------------
C
C     End of Y90SGF
C
C-----------------------------------------------------------------------
      RETURN
      END
