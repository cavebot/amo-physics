      SUBROUTINE Y90ZFF(PRTYPE,MATYPE,SELECT,N,X)
C     MARK 15 RELEASE. NAG COPYRIGHT 1991.
C-----------------------------------------------------------------------
C
C         =====================================
C         *  Y90ZFF :  Return default values  *
C         =====================================
C
C
C  Purpose
C  =======
C
C  Y90ZFF returns default values such as size of matrices, used in the
C  STPs for the F Chapter (Linear Algebra).
C
C  These routine should be tailored by the implementor to best suit
C  the system he/she is working on.
C
C
C  Parameters
C  ==========
C
C  PRTYPE - CHARACTER*1
C           On entry, PRTYPE specifies the type of problem:
C           'E' :  Eigenproblem and SVD.
C                  Symmetric and Hermitian eigenproblems
C                  Unsymmetric eigenproblems
C                  Singular Value Decomposition
C           'F' :  Factorization.
C                  LU Factorization
C                  Cholesky factorization
C                  QR factorization
C           'M' :  Matrix manipulation.
C                  Matrix multiplication
C                  Matrix transposition
C                  Any other matrix manipulations
C           'S' :  Simultaneous equations
C                  Matrix inverse
C                  Symmetric positive definite systems
C                  General systems
C                  Iterative methods
C                  Least square solution
C  MATYPE - CHARACTER*1
C           On entry, MATYPE specifies the type of matrix:
C           'B' :  Banded
C           'D' :  Dense
C           'S' :  Sparse
C  SELECT - CHARACTER*1
C           On entry, SELECT specifies any other additional field:
C           'A' :  Aspect ratio: matrix dimensions ratio
C           'S' :  Problem size
C           'I' :  Problem size for Iterative methods
C           'L' :  Lower half-bandwidth (no diagonal)
C           'R' :  Number of Right-hand sides.
C           'U' :  Upper half-bandwidth (no diagonal)
C  N      - INTEGER
C           On exit, N contains a returned integer value
C  X      - DOUBLE PRECISION
C           On exit, X contains a double precision integer value
C
C-----------------------------------------------------------------------
C
C     A number of internal variables are set up in a DATA statement.
C     Each stores a default value and has a three letter name where
C     the FIRST letter denotes the problem type, the SECOND letter the
C     matrix type, the THIRD letter the final selection field.
C
C     For example, the variable  EDS  will store the Size 'S') of
C     a Dense 'D') Eigenproblem 'E').
C
C-----------------------------------------------------------------------
C     .. Parameters ..
      INTEGER           NMAX
      DOUBLE PRECISION  XMAX
      PARAMETER         (NMAX=80,XMAX=1.5D0)
C     .. Scalar Arguments ..
      DOUBLE PRECISION  X
      INTEGER           N
      CHARACTER*1       MATYPE, PRTYPE, SELECT
C     .. Local Scalars ..
      DOUBLE PRECISION  EBA, EDA, ESA, FBA, FDA, FSA, MBA, MDA, MSA,
     *                  SBA, SDA, SSA
      INTEGER           EBI, EBL, EBS, EBU, EDI, EDS, ESI, ESS, FBI,
     *                  FBL, FBS, FBU, FDI, FDS, FSI, FSS, MBI, MBL,
     *                  MBS, MBU, MDI, MDS, MSI, MSS, SBI, SBL, SBR,
     *                  SBS, SBU, SDI, SDR, SDS, SSI, SSR, SSS
      LOGICAL           ERROR, FIRST
C     .. External Functions ..
      LOGICAL           Y90WAF
      EXTERNAL          Y90WAF
C     .. Save statement ..
      SAVE              FIRST
C     .. Data statements ..
      DATA              FIRST/.TRUE./
      DATA              EDS, EDI, EDA, EBS, EBI, EBL, EBU, EBA, ESS,
     *                  ESI, ESA/8, 8, 1.33D0, 10, 10, 3, 2, 1.33D0, 20,
     *                  20, 1.33D0/
      DATA              FDS, FDI, FDA, FBS, FBI, FBL, FBU, FBA, FSS,
     *                  FSI, FSA/6, 8, 1.33D0, 10, 10, 3, 2, 1.33D0, 20,
     *                  20, 1.33D0/
      DATA              MDS, MDI, MDA, MBS, MBI, MBL, MBU, MBA, MSS,
     *                  MSI, MSA/6, 8, 1.33D0, 6, 6, 2, 2, 1.33D0, 10,
     *                  10, 1.33D0/
      DATA              SDS, SDI, SDR, SDA, SBS, SBI, SBL, SBU, SBR,
     *                  SBA, SSS, SSI, SSR, SSA/6, 8, 2, 1.33D0, 10, 10,
     *                  3, 2, 2, 1.33D0, 12, 20, 2, 1.33D0/
C     .. Executable Statements ..
C-----------------------------------------------------------------------
C
C     1. The first time this routine is entered, check all
C        default values agains maximum allowed (defined as parameters)
C
C-----------------------------------------------------------------------
      ERROR = .TRUE.
      IF (FIRST) THEN
         FIRST = .FALSE.
C
         ERROR = ERROR .AND. (EDS.LE.NMAX) .AND. (EDI.LE.NMAX)
     *           .AND. (EDA.LE.XMAX) .AND. (EBS.LE.NMAX)
     *           .AND. (EBI.LE.NMAX) .AND. (EBL.LE.NMAX)
     *           .AND. (EBU.LE.NMAX) .AND. (EBA.LE.XMAX)
     *           .AND. (ESS.LE.NMAX) .AND. (ESI.LE.NMAX)
     *           .AND. (ESA.LE.XMAX)
C
         ERROR = ERROR .AND. (FDS.LE.NMAX) .AND. (FDI.LE.NMAX)
     *           .AND. (FDA.LE.XMAX) .AND. (FBS.LE.NMAX)
     *           .AND. (FBI.LE.NMAX) .AND. (FBL.LE.NMAX)
     *           .AND. (FBU.LE.NMAX) .AND. (FBA.LE.XMAX)
     *           .AND. (FSS.LE.NMAX) .AND. (FSI.LE.NMAX)
     *           .AND. (FSA.LE.XMAX)
C
         ERROR = ERROR .AND. (MDS.LE.NMAX) .AND. (MDI.LE.NMAX)
     *           .AND. (MDA.LE.XMAX) .AND. (MBS.LE.NMAX)
     *           .AND. (MBI.LE.NMAX) .AND. (MBL.LE.NMAX)
     *           .AND. (MBU.LE.NMAX) .AND. (MBA.LE.XMAX)
     *           .AND. (MSS.LE.NMAX) .AND. (MSI.LE.NMAX)
     *           .AND. (MSA.LE.XMAX)
C
         ERROR = ERROR .AND. (SDS.LE.NMAX) .AND. (SDI.LE.NMAX)
     *           .AND. (SDR.LE.NMAX) .AND. (SDA.LE.XMAX)
     *           .AND. (SBS.LE.NMAX) .AND. (SBI.LE.NMAX)
     *           .AND. (SBL.LE.NMAX) .AND. (SBU.LE.NMAX)
     *           .AND. (SBR.LE.NMAX) .AND. (SBA.LE.XMAX)
     *           .AND. (SSS.LE.NMAX) .AND. (SSI.LE.NMAX)
     *           .AND. (SSR.LE.NMAX) .AND. (SSA.LE.XMAX)
C
      END IF
C-----------------------------------------------------------------------
C
C     2. Decomposition :
C        LU Factorization
C        Cholesky factorization
C        QR factorization
C
C-----------------------------------------------------------------------
      IF (Y90WAF(PRTYPE,'Factorization')) THEN
         IF (Y90WAF(MATYPE,'Banded')) THEN
            IF (Y90WAF(SELECT,'Size')) THEN
               N = FBS
            ELSE IF (Y90WAF(SELECT,'Iterative size')) THEN
               N = FBI
            ELSE IF (Y90WAF(SELECT,'Lower half-bandwidth')) THEN
               N = FBL
            ELSE IF (Y90WAF(SELECT,'Upper half-bandwidth')) THEN
               N = FBU
            ELSE IF (Y90WAF(SELECT,'Aspect ratio')) THEN
               X = FBA
            END IF
         ELSE IF (Y90WAF(MATYPE,'Dense')) THEN
            IF (Y90WAF(SELECT,'Size')) THEN
               N = FDS
            ELSE IF (Y90WAF(SELECT,'Iterative size')) THEN
               N = FDI
            ELSE IF (Y90WAF(SELECT,'Aspect ratio')) THEN
               X = FDA
            END IF
         ELSE IF (Y90WAF(MATYPE,'Sparse')) THEN
            IF (Y90WAF(SELECT,'Size')) THEN
               N = FSS
            ELSE IF (Y90WAF(SELECT,'Iterative size')) THEN
               N = FSI
            ELSE IF (Y90WAF(SELECT,'Aspect ratio')) THEN
               X = FSA
            END IF
         END IF
C-----------------------------------------------------------------------
C
C     3. Eigenproblems and SVD:
C        Symmetric and Hermitian eigenproblems
C        Unsymmetric eigenproblems
C        Singular Value Decomposition
C
C-----------------------------------------------------------------------
      ELSE IF (Y90WAF(PRTYPE,'Eigenproblems and SVD')) THEN
         IF (Y90WAF(MATYPE,'Banded')) THEN
            IF (Y90WAF(SELECT,'Size')) THEN
               N = EBS
            ELSE IF (Y90WAF(SELECT,'Iterative size')) THEN
               N = EBI
            ELSE IF (Y90WAF(SELECT,'Lower half-bandwidth')) THEN
               N = EBL
            ELSE IF (Y90WAF(SELECT,'Upper half-bandwidth')) THEN
               N = EBU
            ELSE IF (Y90WAF(SELECT,'Aspect ratio')) THEN
               X = EBA
            END IF
         ELSE IF (Y90WAF(MATYPE,'Dense')) THEN
            IF (Y90WAF(SELECT,'Size')) THEN
               N = EDS
            ELSE IF (Y90WAF(SELECT,'Iterative size')) THEN
               N = EDI
            ELSE IF (Y90WAF(SELECT,'Aspect ratio')) THEN
               X = EDA
            END IF
         ELSE IF (Y90WAF(MATYPE,'Sparse')) THEN
            IF (Y90WAF(SELECT,'Size')) THEN
               N = ESS
            ELSE IF (Y90WAF(SELECT,'Iterative size')) THEN
               N = ESI
            ELSE IF (Y90WAF(SELECT,'Aspect ratio')) THEN
               X = ESA
            END IF
         END IF
C-----------------------------------------------------------------------
C
C     4. Matrix manipulation
C
C-----------------------------------------------------------------------
      ELSE IF (Y90WAF(PRTYPE,'Manipulation of matrices')) THEN
         IF (Y90WAF(MATYPE,'Banded')) THEN
            IF (Y90WAF(SELECT,'Size')) THEN
               N = MBS
            ELSE IF (Y90WAF(SELECT,'Iterative size')) THEN
               N = MBI
            ELSE IF (Y90WAF(SELECT,'Lower half-bandwidth')) THEN
               N = MBL
            ELSE IF (Y90WAF(SELECT,'Upper half-bandwidth')) THEN
               N = MBU
            ELSE IF (Y90WAF(SELECT,'Aspect ratio')) THEN
               X = MBA
            END IF
         ELSE IF (Y90WAF(MATYPE,'Dense')) THEN
            IF (Y90WAF(SELECT,'Size')) THEN
               N = MDS
            ELSE IF (Y90WAF(SELECT,'Iterative size')) THEN
               N = MDI
            ELSE IF (Y90WAF(SELECT,'Aspect ratio')) THEN
               X = MDA
            END IF
         ELSE IF (Y90WAF(MATYPE,'Sparse')) THEN
            IF (Y90WAF(SELECT,'Size')) THEN
               N = MSS
            ELSE IF (Y90WAF(SELECT,'Iterative size')) THEN
               N = MSI
            ELSE IF (Y90WAF(SELECT,'Aspect ratio')) THEN
               X = MSA
            END IF
         END IF
C-----------------------------------------------------------------------
C
C     5. Solution of simultaneous equations and least squares
C        Symmetric positive definite systems
C        General systems
C        Iterative methods
C        Least square solution
C
C-----------------------------------------------------------------------
      ELSE IF (Y90WAF(PRTYPE,'Simultaneous equations')) THEN
         IF (Y90WAF(MATYPE,'Banded')) THEN
            IF (Y90WAF(SELECT,'Size')) THEN
               N = SBS
            ELSE IF (Y90WAF(SELECT,'Iterative size')) THEN
               N = SBI
            ELSE IF (Y90WAF(SELECT,'Lower half-bandwidth')) THEN
               N = SBL
            ELSE IF (Y90WAF(SELECT,'Upper half-bandwidth')) THEN
               N = SBU
            ELSE IF (Y90WAF(SELECT,'Right-hand sides')) THEN
               N = SBR
            ELSE IF (Y90WAF(SELECT,'Aspect ratio')) THEN
               X = SBA
            END IF
         ELSE IF (Y90WAF(MATYPE,'Dense')) THEN
            IF (Y90WAF(SELECT,'Size')) THEN
               N = SDS
            ELSE IF (Y90WAF(SELECT,'Iterative size')) THEN
               N = SDI
            ELSE IF (Y90WAF(SELECT,'Right-hand sides')) THEN
               N = SBR
            ELSE IF (Y90WAF(SELECT,'Aspect ratio')) THEN
               X = SDA
            END IF
         ELSE IF (Y90WAF(MATYPE,'Sparse')) THEN
            IF (Y90WAF(SELECT,'Size')) THEN
               N = SSS
            ELSE IF (Y90WAF(SELECT,'Iterative size')) THEN
               N = SSI
            ELSE IF (Y90WAF(SELECT,'Right-hand sides')) THEN
               N = SBR
            ELSE IF (Y90WAF(SELECT,'Aspect ratio')) THEN
               X = SSA
            END IF
         END IF
      END IF
C-----------------------------------------------------------------------
C
C     End of Y90ZFF
C
C-----------------------------------------------------------------------
      RETURN
      END
