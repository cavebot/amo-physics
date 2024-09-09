*     F06CLF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      COMPLEX*16       CZERO
      DOUBLE PRECISION EIGHT, EPS, FIVE, FLMAX, FLMIN, FOUR, ONE, RTEPS,
     +                 RTSIX, RTTHR, RTTWO, SEVEN, SIX, STOL, THREE,
     +                 TOL, TWO, ZERO
      INTEGER          N
*     .. Local Arrays ..
      COMPLEX*16       CCORR(15,6), ENTER(15,6), FTRUE(15), X(15), Y(15)
      DOUBLE PRECISION RCORR(15,6)
      LOGICAL          FAILTR(15)
*     .. External Functions ..
      DOUBLE PRECISION X02AJF, X02AMF
      EXTERNAL         X02AJF, X02AMF
*     .. External Subroutines ..
      EXTERNAL         CHECK0, CHECK1, CHECK2
*     .. Intrinsic Functions ..
      INTRINSIC        DCMPLX, SQRT
*     .. Executable Statements ..
      WRITE (NOUT,99999)
      ZERO = 0
      CZERO = DCMPLX(ZERO,ZERO)
      ONE = 1
      TWO = 2
      THREE = 3
      FOUR = 4
      FIVE = 5
      SIX = 6
      SEVEN = 7
      EIGHT = 8
      RTTWO = SQRT(TWO)
      RTTHR = SQRT(THREE)
      RTSIX = SQRT(SIX)
      FLMIN = X02AMF()
      FLMAX = ONE/FLMIN
      EPS = X02AJF()
      RTEPS = SQRT(EPS)
      TOL = EPS**0.666666D+0
      STOL = FLMIN/EPS
*
*     Initialise data for (CDIV) F06CLF tests.
      N = 9
      X(1) = CZERO
      Y(1) = CZERO
      FTRUE(1) = CZERO
      FAILTR(1) = .TRUE.
*
      X(2) = CZERO
      Y(2) = DCMPLX(-FOUR,THREE)
      FTRUE(2) = CZERO
      FAILTR(2) = .FALSE.
*
      X(3) = DCMPLX(FLMAX,-FLMAX)
      Y(3) = DCMPLX(FLMIN,FLMIN)
      FTRUE(3) = DCMPLX(FLMAX,-FLMAX)
      FAILTR(3) = .TRUE.
*
      X(4) = DCMPLX(FOUR,THREE)
      Y(4) = DCMPLX(TWO,-ONE)
      FTRUE(4) = DCMPLX(ONE,TWO)
      FAILTR(4) = .FALSE.
*
      X(5) = DCMPLX(ONE,ONE)
      Y(5) = DCMPLX(TWO*FLMIN,TWO*FLMIN)
      FTRUE(5) = DCMPLX(FLMAX/TWO,ZERO)
      FAILTR(5) = .FALSE.
*
      X(6) = DCMPLX(ONE,ONE)
      Y(6) = DCMPLX(FLMAX,FLMAX)
      FTRUE(6) = DCMPLX(FLMIN,ZERO)
      FAILTR(6) = .FALSE.
*
      X(7) = DCMPLX(ONE,ONE)
      Y(7) = CZERO
      FTRUE(7) = DCMPLX(FLMAX,FLMAX)
      FAILTR(7) = .TRUE.
*
      X(8) = DCMPLX(FLMAX/TWO,FLMAX/TWO)
      Y(8) = DCMPLX(-ONE/TWO,-ONE/FOUR)
      FTRUE(8) = DCMPLX(-FLMAX,-FLMAX)
      FAILTR(8) = .TRUE.
*
      X(9) = DCMPLX(ONE/THREE,ONE/THREE)
      Y(9) = DCMPLX(ONE/THREE,TWO/THREE)
      FTRUE(9) = DCMPLX(THREE/FIVE,-ONE/FIVE)
      FAILTR(9) = .FALSE.
*
*     Perform (CDIV) F06CLF tests.
      CALL CHECK0('F06CLF',N,X,Y,FTRUE,FAILTR,TOL)
*
*     Initialise data for (CROTGC) F06CAF tests.
      N = 5
      ENTER(1,1) = DCMPLX(TWO,ONE)
      ENTER(1,2) = CZERO
      RCORR(1,1) = ONE
      CCORR(1,1) = CZERO
      CCORR(1,2) = DCMPLX(TWO,ONE)
*
      ENTER(2,1) = DCMPLX(-RTTWO/THREE,RTTWO/THREE)
      ENTER(2,2) = DCMPLX(ONE/THREE,ONE/THREE)
      RCORR(2,1) = RTTWO/RTTHR
      CCORR(2,1) = DCMPLX(ZERO,-ONE/RTTHR)
      CCORR(2,2) = DCMPLX(-ONE/RTTHR,ONE/RTTHR)
*
      ENTER(3,1) = DCMPLX(TWO,ONE)
      ENTER(3,2) = DCMPLX(EPS,EPS)
      RCORR(3,1) = ONE
      CCORR(3,1) = CZERO
      CCORR(3,2) = DCMPLX(TWO,ONE)
*
      ENTER(4,1) = DCMPLX(-TWO*EPS,TWO*EPS)
      ENTER(4,2) = DCMPLX(-RTTHR*EPS,-RTTHR*EPS)
      RCORR(4,1) = TWO/SQRT(SEVEN)
      CCORR(4,1) = DCMPLX(ZERO,RTTHR/SQRT(SEVEN))
      CCORR(4,2) = DCMPLX(-SQRT(SEVEN)*EPS,SQRT(SEVEN)*EPS)
*
      ENTER(5,1) = DCMPLX(ONE,ZERO)
      ENTER(5,2) = DCMPLX(ONE,TWO)
      RCORR(5,1) = ONE/RTSIX
      CCORR(5,1) = DCMPLX(ONE/RTSIX,TWO/RTSIX)
      CCORR(5,2) = DCMPLX(RTSIX,ZERO)
*
*     Perform (CROTGC) F06CAF tests.
      CALL CHECK1('F06CAF',N,ENTER,RCORR,CCORR,TOL)
*
*     Initialise data for (CROTGS) F06CBF tests.
      N = 6
      ENTER(1,1) = DCMPLX(ONE,-ONE)
      ENTER(1,2) = CZERO
      RCORR(1,1) = ZERO
      CCORR(1,1) = DCMPLX(ONE/RTTWO,-ONE/RTTWO)
      CCORR(1,2) = DCMPLX(RTTWO,ZERO)
*
      ENTER(2,1) = DCMPLX(RTTWO,ONE)
      ENTER(2,2) = DCMPLX(ONE-ONE/RTTWO,ONE+ONE/RTTWO)
      RCORR(2,1) = ONE/RTTWO
      CCORR(2,1) = DCMPLX(ONE/TWO,-ONE/TWO)
      CCORR(2,2) = DCMPLX(RTTWO-ONE,RTTWO+ONE)
*
      ENTER(3,1) = DCMPLX(RTTWO,ONE)
      ENTER(3,2) = DCMPLX(ONE+RTTWO,ONE-RTTWO)
      RCORR(3,1) = RTTWO/RTTHR
      CCORR(3,1) = DCMPLX(ONE/SQRT(SIX),ONE/SQRT(SIX))
      CCORR(3,2) = DCMPLX(RTTHR*(ONE+RTTWO)/RTTWO,RTTHR*(ONE-RTTWO)
     +             /RTTWO)
*
      ENTER(4,1) = DCMPLX(RTTWO,ONE)
      ENTER(4,2) = DCMPLX(EPS*(ONE+RTTWO)/TWO,EPS*(ONE-RTTWO)/TWO)
      RCORR(4,1) = ZERO
      CCORR(4,1) = DCMPLX(ONE/RTTWO,ONE/RTTWO)
      CCORR(4,2) = DCMPLX(ONE+ONE/RTTWO,ONE/RTTWO-ONE)
*
      ENTER(5,1) = DCMPLX(ONE,ZERO)
      ENTER(5,2) = CZERO
      RCORR(5,1) = ZERO
      CCORR(5,1) = DCMPLX(ONE,ZERO)
      CCORR(5,2) = DCMPLX(ONE,ZERO)
*
      ENTER(6,1) = DCMPLX(ONE,ZERO)
      ENTER(6,2) = DCMPLX(ONE,ZERO)
      RCORR(6,1) = ONE/RTTWO
      CCORR(6,1) = DCMPLX(ONE/RTTWO,ZERO)
      CCORR(6,2) = DCMPLX(RTTWO,ZERO)
*
*     Perform (CROTGS) F06CBF tests.
      CALL CHECK1('F06CBF',N,ENTER,RCORR,CCORR,TOL)
*
*     Initialise data for (CCSG) F06CCF tests.
      N = 4
      ENTER(1,1) = DCMPLX(EPS/TWO,EPS/TWO)
      RCORR(1,1) = ONE
      CCORR(1,1) = CZERO
*
      ENTER(2,1) = DCMPLX(-RTEPS/TWO,RTEPS/TWO)
      RCORR(2,1) = ONE
      CCORR(2,1) = DCMPLX(-RTEPS/TWO,RTEPS/TWO)
*
      ENTER(3,1) = DCMPLX(ONE/EPS,ONE/EPS)
      RCORR(3,1) = EPS/RTTWO
      CCORR(3,1) = DCMPLX(ONE/RTTWO,ONE/RTTWO)
*
      ENTER(4,1) = DCMPLX(THREE/FIVE,-FOUR/FIVE)
      RCORR(4,1) = ONE/RTTWO
      CCORR(4,1) = DCMPLX(THREE/(FIVE*RTTWO),-FOUR/(FIVE*RTTWO))
*
*     Perform (CCSG) F06CCF tests.
      CALL CHECK1('F06CCF',N,ENTER,RCORR,CCORR,TOL)
*
*     Initialise data for (CCSGS) F06CDF tests.
      N = 6
      ENTER(1,1) = DCMPLX(-EPS/TWO,EPS/TWO)
      RCORR(1,1) = ZERO
      CCORR(1,1) = DCMPLX(ONE/RTTWO,ONE/RTTWO)
*
      ENTER(2,1) = DCMPLX(RTEPS/TWO,RTEPS/TWO)
      RCORR(2,1) = RTEPS/RTTWO
      CCORR(2,1) = DCMPLX(ONE/RTTWO,-ONE/RTTWO)
*
      ENTER(3,1) = DCMPLX(ONE/EPS,-ONE/EPS)
      RCORR(3,1) = ONE
      CCORR(3,1) = DCMPLX(EPS/TWO,EPS/TWO)
*
      ENTER(4,1) = DCMPLX(THREE/FIVE,-FOUR/FIVE)
      RCORR(4,1) = ONE/RTTWO
      CCORR(4,1) = DCMPLX(THREE/(FIVE*RTTWO),FOUR/(FIVE*RTTWO))
*
      ENTER(5,1) = DCMPLX(STOL/TWO,STOL/TWO)
      RCORR(5,1) = ZERO
      CCORR(5,1) = DCMPLX(ONE/TWO,ONE/TWO)
*
      ENTER(6,1) = CZERO
      RCORR(6,1) = ZERO
      CCORR(6,1) = DCMPLX(ONE,ZERO)
*
*     Perform (CCSGS) F06CDF tests.
      CALL CHECK1('F06CDF',N,ENTER,RCORR,CCORR,TOL)
*
*     Initialise data for (CROT2) F06CHF tests.
      N = 4
      ENTER(1,1) = DCMPLX(ONE,ZERO)
      ENTER(1,2) = DCMPLX(TWO,-ONE)
      ENTER(1,3) = DCMPLX(-TWO,ZERO)
      ENTER(1,4) = DCMPLX(ONE,ZERO)
      ENTER(1,5) = CZERO
      CCORR(1,1) = DCMPLX(ONE,ZERO)
      CCORR(1,2) = DCMPLX(TWO,-ONE)
      CCORR(1,3) = DCMPLX(-TWO,ZERO)
*
      ENTER(2,1) = DCMPLX(TWO,ZERO)
      ENTER(2,2) = DCMPLX(TWO,ONE)
      ENTER(2,3) = DCMPLX(THREE,ZERO)
      ENTER(2,4) = DCMPLX(-RTTWO,ZERO)
      ENTER(2,5) = DCMPLX(ZERO,ONE)
      CCORR(2,1) = DCMPLX(SEVEN+TWO*RTTWO,ZERO)
      CCORR(2,2) = DCMPLX(SIX,ONE+RTTWO)
      CCORR(2,3) = DCMPLX(EIGHT-TWO*RTTWO,ZERO)
*
      ENTER(3,1) = DCMPLX(TWO,ZERO)
      ENTER(3,2) = DCMPLX(ONE,TWO)
      ENTER(3,3) = DCMPLX(THREE,ZERO)
      ENTER(3,4) = CZERO
      ENTER(3,5) = DCMPLX(ONE,ZERO)
      CCORR(3,1) = DCMPLX(THREE,ZERO)
      CCORR(3,2) = DCMPLX(-ONE,TWO)
      CCORR(3,3) = DCMPLX(TWO,ZERO)
*
      ENTER(4,1) = DCMPLX(TWO,ZERO)
      ENTER(4,2) = DCMPLX(ONE,ONE)
      ENTER(4,3) = DCMPLX(ONE,ZERO)
      ENTER(4,4) = DCMPLX(TWO,ZERO)
      ENTER(4,5) = DCMPLX(ZERO,-RTTHR)
      CCORR(4,1) = DCMPLX(11+FOUR*RTTHR,ZERO)
      CCORR(4,2) = DCMPLX(SEVEN,ONE-TWO*RTTHR)
      CCORR(4,3) = DCMPLX(10-FOUR*RTTHR,ZERO)
*
*     Perform (CROT2) F06CHF tests.
      CALL CHECK2('F06CHF',N,ENTER,CCORR,TOL)
*
      STOP
*
99999 FORMAT (' F06CLF Example Program Results',/1X)
      END
      SUBROUTINE CHECK0(FUNNAM,N,X,Y,FTRUE,FAILTR,TOL)
*     .. Parameters ..
      INTEGER           NOUT
      PARAMETER         (NOUT=6)
*     .. Scalar Arguments ..
      DOUBLE PRECISION  TOL
      INTEGER           N
      CHARACTER*6       FUNNAM
*     .. Array Arguments ..
      COMPLEX*16        FTRUE(N), X(N), Y(N)
      LOGICAL           FAILTR(N)
*     .. Local Scalars ..
      COMPLEX*16        F
      INTEGER           I
      LOGICAL           FAIL, FIRST, MISSED
*     .. External Functions ..
      COMPLEX*16        F06CLF
      LOGICAL           CNDIFF
      EXTERNAL          F06CLF, CNDIFF
*     .. Executable Statements ..
      FIRST = .TRUE.
      WRITE (NOUT,*) ' Testing routine   ', FUNNAM
      DO 20 I = 1, N
         IF (FUNNAM.EQ.'F06CLF') THEN
            F = F06CLF(X(I),Y(I),FAIL)
         ELSE
            WRITE (NOUT,*) ' Shouldn''t be here in CHECK0'
            STOP
         END IF
         MISSED = .NOT. CNDIFF(F,FTRUE(I),TOL)
     +            .OR. FAIL .NEQV. FAILTR(I)
         IF (MISSED) THEN
            IF (FIRST) THEN
               FIRST = .FALSE.
               WRITE (NOUT,*) '               **** FAIL ****'
            END IF
*           Give details of failure here.
            WRITE (NOUT,*) ' entered with ', X(I), Y(I)
            WRITE (NOUT,*) ' returned ', F, ' instead of ', FTRUE(I)
         END IF
   20 CONTINUE
      IF (FIRST) THEN
         WRITE (NOUT,*) '               ---- PASS ----'
      END IF
      WRITE (NOUT,*)
*
      RETURN
      END
      SUBROUTINE CHECK1(FUNNAM,N,ENTER,RCORR,CCORR,TOL)
*     .. Parameters ..
      INTEGER           NOUT
      PARAMETER         (NOUT=6)
*     .. Scalar Arguments ..
      DOUBLE PRECISION  TOL
      INTEGER           N
      CHARACTER*6       FUNNAM
*     .. Array Arguments ..
      COMPLEX*16        CCORR(15,6), ENTER(15,6)
      DOUBLE PRECISION  RCORR(15,6)
*     .. Local Scalars ..
      COMPLEX*16        CEXIT, CEXIT2
      DOUBLE PRECISION  REXIT
      INTEGER           I
      LOGICAL           FIRST, MISSED
*     .. Local Arrays ..
      COMPLEX*16        CPYENT(6)
*     .. External Functions ..
      LOGICAL           CNDIFF, NODIFF
      EXTERNAL          CNDIFF, NODIFF
*     .. External Subroutines ..
      EXTERNAL          F06CAF, F06CBF, F06CCF, F06CDF
*     .. Executable Statements ..
      FIRST = .FALSE.
      WRITE (NOUT,*) ' Testing routine   ', FUNNAM
      DO 20 I = 1, N
         CPYENT(1) = ENTER(I,1)
         IF (FUNNAM.EQ.'F06CAF' .OR. FUNNAM.EQ.'F06CBF') THEN
            CPYENT(2) = ENTER(I,2)
         END IF
         IF (FUNNAM.EQ.'F06CAF') THEN
            CALL F06CAF(ENTER(I,1),ENTER(I,2),REXIT,CEXIT)
            CEXIT2 = ENTER(I,1)
         ELSE IF (FUNNAM.EQ.'F06CBF') THEN
            CALL F06CBF(ENTER(I,1),ENTER(I,2),CEXIT,REXIT)
            CEXIT2 = ENTER(I,1)
         ELSE IF (FUNNAM.EQ.'F06CCF') THEN
            CALL F06CCF(ENTER(I,1),REXIT,CEXIT)
         ELSE IF (FUNNAM.EQ.'F06CDF') THEN
            CALL F06CDF(ENTER(I,1),CEXIT,REXIT)
         ELSE
            WRITE (NOUT,*) ' Shouldn''t be here in CHECK1'
            STOP
         END IF
         MISSED = .NOT. CNDIFF(CEXIT,CCORR(I,1),TOL)
     +            .OR. .NOT. NODIFF(REXIT,RCORR(I,1),TOL)
         IF (FUNNAM.EQ.'F06CAF' .OR. FUNNAM.EQ.'F06CBF') THEN
            MISSED = MISSED .OR. .NOT. CNDIFF(CEXIT2,CCORR(I,2),TOL)
         END IF
         IF (MISSED) THEN
            IF ( .NOT. FIRST) THEN
               FIRST = .TRUE.
               WRITE (NOUT,*) '               **** FAIL ****'
            END IF
*           Give details of failure here.
            IF (FUNNAM.EQ.'F06CAF' .OR. FUNNAM.EQ.'F06CBF') THEN
               WRITE (NOUT,*) ' entered with ', CPYENT(1), CPYENT(2)
            ELSE
               WRITE (NOUT,*) ' entered with ', CPYENT(1)
            END IF
            IF (FUNNAM.EQ.'F06CAF') THEN
               WRITE (NOUT,*) 'returned with ', CEXIT2, REXIT, CEXIT
               WRITE (NOUT,*) '   instead of ', CCORR(I,2), RCORR(I,1),
     +           CCORR(I,1)
            ELSE IF (FUNNAM.EQ.'F06CBF') THEN
               WRITE (NOUT,*) 'returned with ', CEXIT2, CEXIT, REXIT
               WRITE (NOUT,*) '   instead of ', CCORR(I,2), CCORR(I,1),
     +           RCORR(I,1)
            ELSE IF (FUNNAM.EQ.'F06CCF') THEN
               WRITE (NOUT,*) 'returned with ', REXIT, CEXIT
               WRITE (NOUT,*) '   instead of ', RCORR(I,1), CCORR(I,1)
            ELSE IF (FUNNAM.EQ.'F06CDF') THEN
               WRITE (NOUT,*) 'returned with ', CEXIT, REXIT
               WRITE (NOUT,*) '   instead of ', CCORR(I,1), RCORR(I,1)
            END IF
         END IF
   20 CONTINUE
*
      IF ( .NOT. FIRST) WRITE (NOUT,*) '               ---- PASS ----'
      WRITE (NOUT,*)
*
      RETURN
      END
      SUBROUTINE CHECK2(FUNNAM,N,ENTER,CCORR,TOL)
*     .. Parameters ..
      INTEGER           NOUT
      PARAMETER         (NOUT=6)
*     .. Scalar Arguments ..
      DOUBLE PRECISION  TOL
      INTEGER           N
      CHARACTER*6       FUNNAM
*     .. Array Arguments ..
      COMPLEX*16        CCORR(15,6), ENTER(15,6)
*     .. Local Scalars ..
      INTEGER           I, K
      LOGICAL           FIRST, MISSED
*     .. Local Arrays ..
      COMPLEX*16        CEXIT(6), CPYENT(6), CTRUE(6)
*     .. External Functions ..
      LOGICAL           CNDIFV
      EXTERNAL          CNDIFV
*     .. External Subroutines ..
      EXTERNAL          F06CHF
*     .. Intrinsic Functions ..
      INTRINSIC         DBLE
*     .. Executable Statements ..
      FIRST = .FALSE.
      WRITE (NOUT,*) ' Testing routine   ', FUNNAM
      DO 60 I = 1, N
         DO 20 K = 1, 5
            CPYENT(K) = ENTER(I,K)
   20    CONTINUE
         IF (FUNNAM.EQ.'F06CHF') THEN
            CALL F06CHF(ENTER(I,1),ENTER(I,2),ENTER(I,3),DBLE(ENTER(I,4)
     +                  ),ENTER(I,5))
            DO 40 K = 1, 3
               CTRUE(K) = CCORR(I,K)
               CEXIT(K) = ENTER(I,K)
   40       CONTINUE
         ELSE
            WRITE (NOUT,*) ' Shouldn''t be here in CHECK2'
            STOP
         END IF
         MISSED = .NOT. CNDIFV(3,CEXIT,CTRUE,TOL)
         IF (MISSED) THEN
            IF ( .NOT. FIRST) THEN
               FIRST = .TRUE.
               WRITE (NOUT,*) '               **** FAIL ****'
            END IF
*           Give details of failure here.
            WRITE (NOUT,*) ' entered with ', (CPYENT(K),K=1,5)
            WRITE (NOUT,*) 'returned with ', (CEXIT(K),K=1,3)
            WRITE (NOUT,*) '   instead of ', (CTRUE(K),K=1,3)
         END IF
   60 CONTINUE
*
      IF ( .NOT. FIRST) WRITE (NOUT,*) '               ---- PASS ----'
      WRITE (NOUT,*)
*
      RETURN
      END
      LOGICAL FUNCTION NODIFF(SCOMP,STRUE,TOL)
*     Returns .TRUE. if there is no difference between SCOMP and STRUE,
*     to tolerance TOL.
*     .. Parameters ..
      DOUBLE PRECISION        ZERO
      PARAMETER               (ZERO=0.0D+0)
*     .. Scalar Arguments ..
      DOUBLE PRECISION        SCOMP, STRUE, TOL
*     .. Local Scalars ..
      DOUBLE PRECISION        DIF, SC, ST
*     .. Intrinsic Functions ..
      INTRINSIC               ABS, MAX
*     .. Executable Statements ..
      IF (ABS(SCOMP).LT.TOL) THEN
         SC = ZERO
      ELSE
         SC = SCOMP
      END IF
      IF (ABS(STRUE).LT.TOL) THEN
         ST = ZERO
      ELSE
         ST = STRUE
      END IF
      DIF = ABS(SC-ST)
      IF (SC.NE.ZERO .AND. ST.NE.ZERO) DIF = DIF/MAX(ABS(SC),ABS(ST))
      NODIFF = DIF .LE. TOL
      RETURN
      END
      LOGICAL FUNCTION CNDIFF(SCOMP,STRUE,TOL)
*     Returns .TRUE. if there is no difference between SCOMP and STRUE,
*     to tolerance TOL.
*     .. Parameters ..
      DOUBLE PRECISION        ZERO
      PARAMETER               (ZERO=0.0D+0)
*     .. Scalar Arguments ..
      COMPLEX*16              SCOMP, STRUE
      DOUBLE PRECISION        TOL
*     .. Local Scalars ..
      COMPLEX*16              SC
      DOUBLE PRECISION        ABSDIF, ABSMAX, ISC, RSC
*     .. Intrinsic Functions ..
      INTRINSIC               ABS, DIMAG, DCMPLX, MAX, DBLE
*     .. Executable Statements ..
      RSC = DBLE(SCOMP)
      ISC = DIMAG(SCOMP)
      IF (ABS(RSC).LE.TOL .AND. DBLE(STRUE).EQ.ZERO) RSC = ZERO
      IF (ABS(ISC).LE.TOL .AND. DIMAG(STRUE).EQ.ZERO) ISC = ZERO
      SC = DCMPLX(RSC,ISC)
      ABSDIF = ABS(SC-STRUE)
*     ABSMAX = MAX(ABS(SC),ABS(STRUE))
*     CNDIFF = ABSDIF .LE. TOL*ABSMAX
*     Replace the two lines above by the two lines below, to avoid
*     possible overflow in ABS when SC or STRUE is very large.
      ABSMAX = MAX(ABS(SC*TOL),ABS(STRUE*TOL))
      CNDIFF = ABSDIF .LE. ABSMAX
      RETURN
      END
      LOGICAL FUNCTION CNDIFV(LEN,SCOMP,STRUE,TOL)
*     Returns .TRUE. if there is no difference between arrays SCOMP and
*     STRUE, to tolerance TOL. Differences are checked componentwise,
*     and real and imaginary parts are treated separately.
*     .. Scalar Arguments ..
      DOUBLE PRECISION        TOL
      INTEGER                 LEN
*     .. Array Arguments ..
      COMPLEX*16              SCOMP(*), STRUE(*)
*     .. Local Scalars ..
      INTEGER                 I
*     .. External Functions ..
      LOGICAL                 CNDIFF
      EXTERNAL                CNDIFF
*     .. Executable Statements ..
      CNDIFV = .TRUE.
      DO 20 I = 1, LEN
         CNDIFV = CNDIFV .AND. CNDIFF(SCOMP(I),STRUE(I),TOL)
   20 CONTINUE
      RETURN
      END
