      SUBROUTINE E02AKF(NP1,XMIN,XMAX,A,IA1,LA,X,RESULT,IFAIL)
C     MARK 8 RELEASE. NAG COPYRIGHT 1979.
C     MARK 11.5(F77) REVISED. (SEPT 1985.)
C
C     * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C     NPL DATA FITTING LIBRARY ROUTINE TVAL1C
C
C     CREATED 19/3/79    UPDATED 6/7/79    RELEASE NO. 00/04.
C
C     AUTHORS.. GERALD T ANTHONY, MAURICE G COX, BETTY CURTIS
C     AND J GEOFFREY HAYES.
C     NATIONAL PHYSICAL LABORATORY
C     TEDDINGTON, MIDDLESEX, ENGLAND.
C
C     * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C     INPUT PARAMETERS
C        NP1      NP1 = N + 1. N IS THE DEGREE OF THE
C                 CHEBYSHEV SERIES
C        XMIN     MINIMUM VALUE OF X
C        XMAX     MAXIMUM VALUE OF X
C        A        THE ARRAY WHERE THE COEFFICIENTS ARE STORED
C        IA1      THE ADDRESS INCREMENT OF A
C        LA       DIMENSION OF A
C        X        UNNORMALIZED ARGUMENT IN THE RANGE (XMIN, XMAX)
C
C     OUTPUT PARAMETERS
C        RESULT   VALUE OF THE SUMMATION
C        IFAIL    ERROR INDICATOR
C
C     NP1 CHEBYSHEV COEFFICIENTS A0, A1, ..., AN, ARE
C     STORED IN THE ARRAY A IN POSITIONS 1, 1+IA1, 1+2*IA1, ...,
C     1+N*IA1, WHERE N = NP1 - 1.
C     IA1 MUST NOT BE NEGATIVE.
C     LA MUST BE AT LEAST EQUAL TO 1 + N*IA1.
C     THE VALUE OF THE POLYNOMIAL OF DEGREE N
C     A0T0(XCAP)/2 + A1T1(XCAP) + A2T2(XCAP) + + ... + ANTN(XCAP),
C     IS CALCULATED FOR THE ARGUMENT XCAP, WHERE XCAP IS
C     THE NORMALIZED VALUE OF X IN THE RANGE (XMIN, XMAX),
C     STORING IT IN RESULT.
C     UNLESS THE ROUTINE DETECTS AN ERROR, IFAIL CONTAINS
C     ZERO ON EXIT.
C     IFAIL = 1 INDICATES AT LEAST ONE OF THE RESTRICTIONS ON
C        INPUT PARAMETERS IS VIOLATED - IE
C     NP1 .GT. 0
C     IA1 .GE. 0
C     LA .GE. 1 + N * IA1
C     XMIN .LT. XMAX
C     IFAIL = 2 INDICATES THAT
C     X DOES NOT SATISFY THE RESTRICTION XMIN .LE. X .LE. XMAX.
C     THE RECURRENCE RELATION BY CLENSHAW, MODIFIED BY REINSCH
C     AND GENTLEMAN, IS USED.
C
C     .. Parameters ..
      CHARACTER*6       SRNAME
      PARAMETER         (SRNAME='E02AKF')
C     .. Scalar Arguments ..
      DOUBLE PRECISION  RESULT, X, XMAX, XMIN
      INTEGER           IA1, IFAIL, LA, NP1
C     .. Array Arguments ..
      DOUBLE PRECISION  A(LA)
C     .. Local Scalars ..
      DOUBLE PRECISION  XCAP
      INTEGER           IERROR
C     .. Local Arrays ..
      CHARACTER*1       P01REC(1)
C     .. External Functions ..
      INTEGER           P01ABF
      EXTERNAL          P01ABF
C     .. External Subroutines ..
      EXTERNAL          E02AKY, E02AKZ
C     .. Executable Statements ..
      IERROR = 1
      IF (NP1.LT.1) GO TO 20
      IF (IA1.LT.1) GO TO 20
      IF (LA.LT.1+(NP1-1)*IA1) GO TO 20
      IF (XMAX.LE.XMIN) GO TO 20
      IERROR = IERROR + 1
      IF ((X.GT.XMAX) .OR. (X.LT.XMIN)) GO TO 20
      IERROR = 0
      CALL E02AKY(XMIN,XMAX,X,XCAP)
      CALL E02AKZ(NP1,A,IA1,LA,XCAP,RESULT)
   20 IFAIL = P01ABF(IFAIL,IERROR,SRNAME,0,P01REC)
      RETURN
C
C     END E02AKF
C
      END
