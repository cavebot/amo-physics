      SUBROUTINE E04ZCF(N,M,LA,CON,FUN,C,A,F,G,X,W,LW,IFAIL)
C     MARK 11 RELEASE. NAG COPYRIGHT 1983.
C     MARK 11.5(F77) REVISED. (SEPT 1985.)
C     MARK 13 REVISED. USE OF MARK 12 X02 FUNCTIONS (APR 1988).
C     MARK 14 REVISED. IER-810 (DEC 1989).
C     MARK 15B REVISED. IER-952 (NOV 1991).
C
C     THIS VERSION MODIFIED (FROM E04ZAF) FOR USE WITH ROUTINES
C     E04VCF AND E04VDF.  S.J.HAMMARLING - 25 JULY 1983.
C
C     **************************************************************
C
C     E04ZCF CHECKS THAT USER-SUPPLIED ROUTINES FOR EVALUATING AN
C     OBJECTIVE FUNCTION, CONSTRAINT FUNCTIONS AND THEIR FIRST
C     DERIVATIVES PRODUCE DERIVATIVE VALUES WHICH ARE CONSISTENT
C     WITH THE FUNCTION AND CONSTRAINT VALUES CALCULATED.
C
C     THE ROUTINE IS ESSENTIALLY IDENTICAL TO THE SUBROUTINE CHKNCD
C     IN THE NPL ALGORITHMS LIBRARY (REF. NO. E4/66/F). W(I), I = 1,
C     2, . . . , 4*N + M + N*M ARE USED AS WORKSPACE. (NOTE THAT,
C     FOR CONSISTENCY WITH OTHER E04 DOCUMENTATION, THE NAME FUNCT
C     IS USED INSTEAD OF SFUN IN THE WRITE-UP.)
C
C     PHILIP E. GILL, ENID M. R. LONG, WALTER MURRAY AND SUSAN M.
C     PICKEN, D.N.A.C., NATIONAL PHYSICAL LABORATORY, ENGLAND.
C
C     **************************************************************
C
C     CON, FUN
C
C     A MACHINE DEPENDENT CONSTANT IS SET HERE. EPSMCH IS THE
C     SMALLEST POSITIVE REAL NUMBER SUCH THAT 1.0 + EPSMCH .GT. 1.0.
C
C     .. Parameters ..
      CHARACTER*6       SRNAME
      PARAMETER         (SRNAME='E04ZCF')
C     .. Scalar Arguments ..
      DOUBLE PRECISION  F
      INTEGER           IFAIL, LA, LW, M, N
C     .. Array Arguments ..
      DOUBLE PRECISION  A(LA,N), C(LA), G(N), W(LW), X(N)
C     .. Subroutine Arguments ..
      EXTERNAL          CON, FUN
C     .. Local Scalars ..
      DOUBLE PRECISION  DIFF, EPSMCH, ERROR, FD, GTP, H, SUM
      INTEGER           I, IAD, ICD, ICDJ, IGD, IP, IPI, IX, IXI, J, K,
     *                  NSTATE, NWHY
C     .. Local Arrays ..
      CHARACTER*1       P01REC(1)
C     .. External Functions ..
      DOUBLE PRECISION  DDOT, X02AJF
      INTEGER           P01ABF
      EXTERNAL          DDOT, X02AJF, P01ABF
C     .. External Subroutines ..
      EXTERNAL          E04HCZ
C     .. Intrinsic Functions ..
      INTRINSIC         MAX, SQRT
C     .. Executable Statements ..
      EPSMCH = X02AJF()
      H = SQRT(EPSMCH)
      NWHY = 1
      IF (N.LT.1 .OR. M.LT.0 .OR. LA.LT.MAX(1,M) .OR. LW.LT.N*(4+LA)+M)
     *    GO TO 140
C
C     THE FUNCTION, GRADIENT, CONSTRAINT AND CONSTRAINT DERIVATIVES
C     ARE EVALUATED AT X.
C
      NSTATE = 1
      IF (M.EQ.0) GO TO 20
      NWHY = 2
      CALL CON(NWHY,M,N,LA,X,C,A,NSTATE)
      IF (NWHY.LT.0) GO TO 140
   20 CONTINUE
      NWHY = 2
      CALL FUN(NWHY,N,X,F,G,NSTATE)
      IF (NWHY.LT.0) GO TO 140
      NSTATE = 0
C
C     TWO ORTHOGONAL VECTORS ARE SET UP.
C
      CALL E04HCZ(N,W(1),W(N+1))
      IP = 1
C
C     THIS LOOP IS OBEYED FOR EACH OF THE ORTHOGONAL VECTORS UNLESS
C     N = 1, WHEN THE LOOP IS OBEYED ONLY ONCE.
C
      DO 120 K = 1, 2
         IF (N.EQ.1 .AND. K.EQ.1) GO TO 110
C
C        THE CONSTRAINTS AND FUNCTION ARE EVALUATED AT X + H*P, WHERE
C        H = SQRT(EPSMCH) AND P IS ONE OF THE ORTHOGONAL VECTORS.
C
         IX = 2*N + 1
         DO 40 I = 1, N
            IXI = IX + I - 1
            IPI = IP + I - 1
            W(IXI) = X(I) + H*W(IPI)
   40    CONTINUE
C
         IF (M.EQ.0) GO TO 100
         ICD = 4*N + 1
         IAD = ICD + M
         NWHY = 2
         CALL CON(NWHY,M,N,LA,W(IX),W(ICD),W(IAD),NSTATE)
         IF (NWHY.LT.0) GO TO 140
         DO 80 J = 1, M
            SUM = 0.0D+0
            DO 60 I = 1, N
               IPI = IP + I - 1
               SUM = SUM + W(IPI)*A(J,I)
   60       CONTINUE
            ICDJ = ICD + J - 1
C
C           EACH COLUMN OF THE M BY N JACOBIAN MATRIX OF THE FIRST
C           DERIVATIVES OF THE CONSTRAINTS PROJECTED ONTO P IS COMPARED
C           WITH A FINITE DIFFERENCE APPROXIMATION AND A FAILURE IS
C           SET IF THEY DISAGREE.
C
            DIFF = (W(ICDJ)-C(J))/H
            ERROR = DIFF - SUM
            NWHY = J + 2
            IF (ERROR*ERROR.GE.H*(SUM*SUM+1.0D+0)) GO TO 140
   80    CONTINUE
  100    CONTINUE
C
         IGD = 3*N + 1
         NWHY = 2
         CALL FUN(NWHY,N,W(IX),FD,W(IGD),NSTATE)
         IF (NWHY.LT.0) GO TO 140
         GTP = DDOT(N,W(IP),1,G,1)
C
C        THE GRADIENT PROJECTED ONTO P IS COMPARED WITH A FINITE
C        DIFFERENCE APPROXIMATION AND A FAILURE IS SET IF THEY
C        DISAGREE.
C
         DIFF = (FD-F)/H
         ERROR = DIFF - GTP
         NWHY = 2
         IF (ERROR*ERROR.GE.H*(GTP*GTP+1.0D+0)) GO TO 140
  110    CONTINUE
         IP = N + 1
  120 CONTINUE
      NWHY = 0
  140 IF (NWHY.NE.0) GO TO 160
      IFAIL = 0
      RETURN
  160 IFAIL = P01ABF(IFAIL,NWHY,SRNAME,0,P01REC)
      RETURN
C
C     END OF E04ZCF (CHKNCD)
C
      END
