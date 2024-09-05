      SUBROUTINE E04LAF(N,IBOUND,BL,BU,X,F,G,IW,LIW,W,LW,IFAIL)
C
C     MARK 6 RELEASE NAG COPYRIGHT 1977
C     MARK 11.5(F77) REVISED. (SEPT 1985.)
C     MARK 13 REVISED. USE OF MARK 12 X02 FUNCTIONS (APR 1988).
C     MARK 13A REVISED. IER-632 (APR 1988).
C     MARK 14 REVISED. IER-818 (DEC 1989).
C
C     **************************************************************
C
C     E04LAF IS AN EASY-TO-USE MODIFIED NEWTON ALGORITHM FOR FINDING
C     A MINIMUM OF A FUNCTION F(X1, X2, . . . , XN), SUBJECT TO
C     FIXED UPPER AND LOWER BOUNDS ON THE INDEPENDENT VARIABLES X1,
C     X2, . . . , XN , WHEN FIRST AND SECOND DERIVATIVES ARE
C     AVAILABLE.
C
C     GIVEN AN INITIAL ESTIMATE OF THE POSITION OF A CONSTRAINED
C     MINIMUM, THE ROUTINE ATTEMPTS TO COMPUTE THE POSITION OF THE
C     MINIMUM AND THE CORRESPONDING FUNCTION AND GRADIENT VALUES. IT
C     IS ASSUMED THAT F IS TWICE CONTINUOUSLY-DIFFERENTIABLE.
C
C     THE ROUTINE IS ESSENTIALLY IDENTICAL TO THE SUBROUTINE BCSDN2
C     IN THE NPL ALGORITHMS LIBRARY (REF. E4/46/F) AND CALLS E04LBR
C     WITH SUITABLE DEFAULT SETTINGS FOR PARAMETERS.
C
C     N.B. FUNCT2 AND HESS2 ARE DESIGNATED NAMES.
C     ------------------------------------------
C
C     THE REAL ARRAY W, USED AS WORKSPACE BY E04LAF, MUST BE
C     DIMENSIONED AT LEAST N*(N + 7), OR 10 IF N = 1. THE INTEGER
C     ARRAY IW, ALSO USED AS WORKSPACE BY E04LAF, MUST BE
C     DIMENSIONED AT LEAST (N + 2).
C
C     PHILIP E. GILL, WALTER MURRAY, SUSAN M. PICKEN, MARGARET H.
C     WRIGHT AND ENID M. R. LONG, D.N.A.C., NATIONAL PHYSICAL
C     LABORATORY, ENGLAND
C
C     **************************************************************
C
C
C     A MACHINE-DEPENDENT CONSTANT IS SET HERE. EPSMCH IS THE
C     SMALLEST POSITIVE REAL NUMBER SUCH THAT 1 + EPSMCH .GT. 1.
C
C     .. Parameters ..
      CHARACTER*6       SRNAME
      PARAMETER         (SRNAME='E04LAF')
C     .. Scalar Arguments ..
      DOUBLE PRECISION  F
      INTEGER           IBOUND, IFAIL, LIW, LW, N
C     .. Array Arguments ..
      DOUBLE PRECISION  BL(N), BU(N), G(N), W(LW), X(N)
      INTEGER           IW(LIW)
C     .. Local Scalars ..
      DOUBLE PRECISION  BNDMAX, EPSMCH, ETA, GTG, PEPS, RTEPS, STEPMX,
     *                  U, XI, XTOL
      INTEGER           I, IPRINT, J, KIW, KW1, KW2, LH, MAXFUN, NWHY
C     .. Local Arrays ..
      CHARACTER*1       P01REC(1)
C     .. External Functions ..
      DOUBLE PRECISION  DNRM2, X02AJF
      INTEGER           P01ABF
      EXTERNAL          DNRM2, X02AJF, P01ABF
C     .. External Subroutines ..
      EXTERNAL          E04CGZ, E04DEZ, E04EBZ, E04HCF, E04HDF, E04LBR,
     *                  E04LBS
C     .. Intrinsic Functions ..
      INTRINSIC         ABS, SQRT
C     .. Executable Statements ..
      EPSMCH = X02AJF()
C
      RTEPS = SQRT(EPSMCH)
      PEPS = EPSMCH**0.66666D+0
      NWHY = 1
      IF (N.LE.0 .OR. IBOUND.LT.0 .OR. IBOUND.GT.3) GO TO 120
C
C     COMPUTE THE LENGTH OF THE ARRAY WHICH HOLDS THE HESSIAN
C     MATRIX.
C
      LH = N*(N-1)/2
      IF (LH.EQ.0) LH = 1
C
C     CHECK THAT THERE IS SUFFICIENT WORKSPACE.
C
      IF (LW.LT.8*N+LH+LH) GO TO 120
      IF (LIW.LT.N+2) GO TO 120
C
C     IF APPROPRIATE, E04LBR WILL CHECK THAT BL(I) .LE. BU(I)
C
      NWHY = 0
C
C     SUPPRESS THE PRINT FREQUENCY.
C
      IPRINT = 0
C
C     ONLY 50*N FUNCTION EVALUATIONS ARE ALLOWED.
C
      MAXFUN = 50*N
C
C     SET THE LINEAR SEARCH PARAMETER.
C
      ETA = 9.0D-1
      IF (N.EQ.1) ETA = 0.0D+0
C
C     SPECIFY THE OVERALL CONVERGENCE CRITERION.
C
      XTOL = 1.0D+1*RTEPS
C
C     SPECIFY THE BOUND ON THE STEP-LENGTH.
C
      STEPMX = DNRM2(N,X,1)
      IF (STEPMX.LT.1.0D+1) STEPMX = 1.0D+1
C
C     COMPUTE THE ADDRESSES FOR THE ARRAYS USED IN E04LBR.
C
      KW1 = 7*N + LH + 1
      KW2 = KW1 + LH
      KIW = 3
C
C     CHECK THAT THE GRADIENT VECTOR HAS BEEN FORMED CORRECTLY BY
C     CALLING E04HCF WITH SOFT FAILURE OPTION.
C
      NWHY = 1
      CALL E04HCF(N,E04DEZ,X,F,G,IW,LIW,W,LW,NWHY)
      IF (NWHY.EQ.0) GO TO 20
C
C     IF NWHY = 2, THE USER HAS MADE AN ERROR.
C
      IF (NWHY.EQ.2) NWHY = 10
      GO TO 120
C
C     CHECK THAT THE HESSIAN MATRIX HAS BEEN FORMED CORRECTLY BY
C     CALLING E04HDF WITH SOFT FAILURE OPTION.
C
   20 NWHY = 1
      CALL E04HDF(N,E04DEZ,E04EBZ,X,G,W(KW1),LH,W(KW2),IW,LIW,W,LW,NWHY)
      IF (NWHY.EQ.0) GO TO 40
C
C     IF NWHY = 2, THE USER HAS MADE AN ERROR.
C
      IF (NWHY.EQ.2) NWHY = 11
      GO TO 120
C
C     CALL S-ROUTINE E04LBR TO MINIMIZE THE FUNCTION.
C
   40 CALL E04LBR(N,E04DEZ,E04EBZ,.TRUE.,E04CGZ,IPRINT,E04LBS,MAXFUN,
     *            ETA,XTOL,0.0D+0,STEPMX,IBOUND,BL,BU,X,W(KW1),LH,W(KW2)
     *            ,IW(KIW),F,G,IW,LIW,W,LW,NWHY)
      IF (NWHY.LE.2) GO TO 80
C
C     IF THE LINEAR SEARCH HAS FAILED, ATTEMPT TO DETERMINE WHETHER
C     THE FAILURE IS DUE TO TOO SMALL A SETTING FOR XTOL.
C
      U = 1.0D+0 + ABS(F)
      U = PEPS*U*U
      GTG = 0.0D+0
      J = KIW
      DO 60 I = 1, N
         IF (IW(J).GT.0) GTG = GTG + G(I)**2
         J = J + 1
   60 CONTINUE
C
C     THE VALUES 3, 5, 6, 7, 8 FOR NWHY HAVE THE FOLLOWING
C     SIGNIFICANCE
C
C     NWHY = 5  -  THE MINIMIZATION HAS PROBABLY WORKED.
C     NWHY = 6  -  THE MINIMIZATION HAS POSSIBLY WORKED.
C     NWHY = 7  -  THE MINIMIZATION IS UNLIKELY TO HAVE WORKED.
C     NWHY = 8  -  THE MINIMIZATION IS VERY UNLIKELY TO HAVE WORKED.
C     NWHY = 3  -  THE MINIMIZATION HAS FAILED.
C
      IF (NWHY.EQ.5) NWHY = 3
      IF (GTG.LE.1.0D+3*U) NWHY = 8
      IF (GTG.LE.1.0D+2*U) NWHY = 7
      IF (GTG.LE.1.0D+1*U) NWHY = 6
      IF (GTG.LE.U) NWHY = 5
   80 CONTINUE
C
C     TELL THE USER IF ANY VARIABLE IS LARGE AND NEGATIVE OR LARGE
C     AND POSITIVE. (THIS WOULD OCCUR, IN PARTICULAR, IF ANY X(I)
C     REACHED AN ARTIFICIAL BOUND SET BY E04JBL.)
C
      BNDMAX = 0.99999D+6
      DO 100 I = 1, N
         XI = X(I)
         IF (XI.LT.BNDMAX .AND. XI.GT.-BNDMAX) GO TO 100
         NWHY = 9
         GO TO 120
  100 CONTINUE
C
C     TERMINATION OF ALGORITHM
C
  120 IF (NWHY.NE.0) GO TO 140
      IFAIL = 0
      RETURN
  140 IFAIL = P01ABF(IFAIL,NWHY,SRNAME,0,P01REC)
      RETURN
C
C     END OF E04LAF (BCSDN2)
C
      END
