*     F06BLF Example Program Text
*     Mark 15 Revised. NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION EPS, FIVE, FLMAX, FLMIN, FOUR, ONE, RTEPS, RTTHR,
     +                 RTTWO, THREE, TOL, TWO, ZERO
      INTEGER          N, NCORR, NENTER
      LOGICAL          PFAIL
*     .. Local Arrays ..
      DOUBLE PRECISION CORR(6,6), ENTER(6,6), FTRUE(6), X(6), Y(6), Z(6)
      LOGICAL          FAILTR(6)
*     .. External Functions ..
      DOUBLE PRECISION X02AJF, X02AMF
      EXTERNAL         X02AJF, X02AMF
*     .. External Subroutines ..
      EXTERNAL         CHECK0, CHECK1
*     .. Intrinsic Functions ..
      INTRINSIC        SQRT
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F06BLF Example Program Results'
      ZERO = 0
      ONE = 1
      TWO = 2
      THREE = 3
      FOUR = 4
      FIVE = 5
      RTTWO = SQRT(TWO)
      RTTHR = SQRT(THREE)
      FLMIN = X02AMF()
      FLMAX = ONE/FLMIN
      EPS = X02AJF()
      RTEPS = SQRT(EPS)
      TOL = EPS**0.666666D+0
*
*     Initialise 5 sets of data for (SDIV) F06BLF tests.
      N = 5
      X(1) = ONE
      Y(1) = -ONE
      FTRUE(1) = -ONE
      FAILTR(1) = .FALSE.
      X(2) = ZERO
      Y(2) = ONE
      FTRUE(2) = ZERO
      FAILTR(2) = .FALSE.
      X(3) = ONE
      Y(3) = ZERO
      FTRUE(3) = FLMAX
      FAILTR(3) = .TRUE.
      X(4) = FLMIN
      Y(4) = FLMAX
      FTRUE(4) = ZERO
      FAILTR(4) = .FALSE.
      X(5) = -FLMAX
      Y(5) = FLMIN
      FTRUE(5) = -FLMAX
      FAILTR(5) = .TRUE.
*     Perform (SDIV) F06BLF tests.
      CALL CHECK0('F06BLF',N,X,Y,Z,FTRUE,FAILTR,TOL)
*
*     Initialise 5 sets of data for (SNORM) F06BMF tests.
      N = 5
      X(1) = ONE
      Y(1) = FLMAX
      FTRUE(1) = SQRT(FLMAX)
      X(2) = ONE/THREE
      Y(2) = 9
      FTRUE(2) = ONE
      X(3) = FLMAX
      Y(3) = FLMAX
      FTRUE(3) = FLMAX
      X(4) = ZERO
      Y(4) = FLMAX
      FTRUE(4) = ZERO
      X(5) = 10
      Y(5) = 100
      FTRUE(5) = 100
*     Perform (SNORM) F06BMF tests.
      CALL CHECK0('F06BMF',N,X,Y,Z,FTRUE,FAILTR,TOL)
*
*     Initialise 5 sets of data for (SSHIFT) F06BPF tests.
      N = 5
      X(1) = ZERO
      Y(1) = ZERO
      Z(1) = ZERO
      FTRUE(1) = ZERO
      X(2) = FLMAX/TWO
      Y(2) = SQRT(EPS)
      Z(2) = -FLMAX/TWO
      FTRUE(2) = -FLMAX/TWO
      X(3) = ONE + EPS
      Y(3) = 4
      Z(3) = ONE
      FTRUE(3) = -THREE
      X(4) = -ONE
      Y(4) = ONE
      Z(4) = ONE
      FTRUE(4) = RTTWO
      X(5) = ONE
      Y(5) = EPS
      Z(5) = -ONE
      FTRUE(5) = -ONE
*     Perform (SSHIFT) F06BPF tests.
      CALL CHECK0('F06BPF',N,X,Y,Z,FTRUE,FAILTR,TOL)
*
*     Initialise 6 sets of data for (SPYTH) F06BNF tests.
      N = 6
      X(1) = 3
      Y(1) = 4
      FTRUE(1) = 5
      X(2) = 0.3D0
      Y(2) = 0.4D0
      FTRUE(2) = 0.5D0
      X(3) = FLMAX/TWO
      Y(3) = FLMAX/TWO
      FTRUE(3) = FLMAX/RTTWO
      X(4) = ZERO
      Y(4) = FLMAX
      FTRUE(4) = FLMAX
      X(5) = -FLMAX/TWO
      Y(5) = -FLMAX/TWO
      FTRUE(5) = FLMAX/RTTWO
      X(6) = TWO
      Y(6) = ZERO
      FTRUE(6) = TWO
*     Perform (SPYTH) F06BNF tests.
      CALL CHECK0('F06BNF',N,X,Y,Z,FTRUE,FAILTR,TOL)
*
*     Initialise 6 sets of data for (SCSG) F06BCF tests.
      N = 6
      NENTER = 1
      NCORR = 2
      ENTER(1,1) = ZERO
      CORR(1,1) = ONE
      CORR(1,2) = ZERO
      ENTER(2,1) = -FLMAX
      CORR(2,1) = ZERO
      CORR(2,2) = -ONE
      ENTER(3,1) = ONE/RTTHR
      CORR(3,1) = RTTHR/TWO
      CORR(3,2) = ONE/TWO
      ENTER(4,1) = ONE
      CORR(4,1) = ONE/RTTWO
      CORR(4,2) = ONE/RTTWO
      ENTER(5,1) = RTEPS/TWO
      CORR(5,1) = ONE
      CORR(5,2) = RTEPS/TWO
      ENTER(6,1) = TWO/RTEPS
      CORR(6,1) = RTEPS/TWO
      CORR(6,2) = ONE
*     Perform (SCSG) F06BCF tests.
      PFAIL = .FALSE.
      CALL CHECK1('F06BCF',N,NENTER,ENTER,NCORR,CORR,TOL,.TRUE.,.TRUE.,
     +            PFAIL,'X')
*
*     Initialise 4 sets of data for (SROT2) F06BHF tests.
      N = 4
      NENTER = 5
      NCORR = 3
      ENTER(1,1) = ONE
      ENTER(1,2) = ONE
      ENTER(1,3) = ONE
      ENTER(1,4) = ONE
      ENTER(1,5) = ZERO
      CORR(1,1) = ONE
      CORR(1,2) = ONE
      CORR(1,3) = ONE
      ENTER(2,1) = ONE
      ENTER(2,2) = TWO
      ENTER(2,3) = THREE
      ENTER(2,4) = ZERO
      ENTER(2,5) = ONE
      CORR(2,1) = THREE
      CORR(2,2) = -TWO
      CORR(2,3) = ONE
      ENTER(3,1) = THREE/TWO
      ENTER(3,2) = -ONE/TWO
      ENTER(3,3) = THREE/TWO
      ENTER(3,4) = ONE/RTTWO
      ENTER(3,5) = ONE/RTTWO
      CORR(3,1) = ONE
      CORR(3,2) = ZERO
      CORR(3,3) = TWO
      ENTER(4,1) = ((RTTHR-ONE)/TWO)**2
      ENTER(4,2) = ONE/TWO
      ENTER(4,3) = ((RTTHR+ONE)/TWO)**2
      ENTER(4,4) = RTTHR/TWO
      ENTER(4,5) = ONE/TWO
      CORR(4,1) = ONE
      CORR(4,2) = ONE
      CORR(4,3) = ONE
*     Perform (SROT2) F06BHF tests.
      PFAIL = .FALSE.
      CALL CHECK1('F06BHF',N,NENTER,ENTER,NCORR,CORR,TOL,.TRUE.,.TRUE.,
     +            PFAIL,'X')
*
*     Initialise 6 sets of data for (SROTGC) F06BAF tests.
      N = 6
      NENTER = 2
      NCORR = 4
      ENTER(1,1) = ONE
      ENTER(1,2) = ZERO
      CORR(1,1) = ONE
      CORR(1,2) = ZERO
      CORR(1,3) = ONE
      CORR(1,4) = ZERO
      ENTER(2,1) = ONE
      ENTER(2,2) = ONE
      CORR(2,1) = RTTWO
      CORR(2,2) = ONE
      CORR(2,3) = ONE/RTTWO
      CORR(2,4) = ONE/RTTWO
      ENTER(3,1) = -ONE
      ENTER(3,2) = EPS
      CORR(3,1) = -ONE
      CORR(3,2) = -EPS
      CORR(3,3) = ONE
      CORR(3,4) = ZERO
      ENTER(4,1) = EPS
      ENTER(4,2) = -ONE
      CORR(4,1) = ONE
      CORR(4,2) = -ONE/EPS
      CORR(4,3) = ZERO
      CORR(4,4) = -ONE
      ENTER(5,1) = RTTWO*SQRT(FLMAX)
      ENTER(5,2) = RTTWO*SQRT(FLMAX)
      CORR(5,1) = TWO*SQRT(FLMAX)
      CORR(5,2) = ONE
      CORR(5,3) = ONE/RTTWO
      CORR(5,4) = ONE/RTTWO
      ENTER(6,1) = ZERO
      ENTER(6,2) = -ONE
      CORR(6,1) = ONE
      CORR(6,2) = -FLMAX
      CORR(6,3) = ZERO
      CORR(6,4) = -ONE
*     Perform (SROTGC) F06BAF tests.
      PFAIL = .FALSE.
      CALL CHECK1('F06BAF',N,NENTER,ENTER,NCORR,CORR,TOL,.TRUE.,.TRUE.,
     +            PFAIL,'X')
*
*     Initialise 5 sets of data for (SROTJ) F06BEF tests.
      N = 5
      NENTER = 3
      NCORR = 3
      ENTER(1,1) = ONE
      ENTER(1,2) = ZERO
      ENTER(1,3) = ONE
      CORR(1,1) = ONE
*     CORR (*, 2) is not checked.
      CORR(1,3) = ONE
      ENTER(2,1) = TWO
      ENTER(2,2) = TWO
      ENTER(2,3) = 5
      CORR(2,1) = ONE
      CORR(2,3) = 6
      ENTER(3,1) = ONE
      ENTER(3,2) = 100
      ENTER(3,3) = -44
      CORR(3,1) = 81
      CORR(3,3) = -124
      ENTER(4,1) = -99
      ENTER(4,2) = 99
      ENTER(4,3) = -59
      CORR(4,1) = -180
      CORR(4,3) = 22
      ENTER(5,1) = ONE
      ENTER(5,2) = FOUR
      ENTER(5,3) = (TWO*EPS) + ONE
      CORR(5,1) = -THREE
      CORR(5,3) = (TWO*EPS) + FIVE
*     Perform (SROTJ) F06BEF tests with JOB = 'B'.
      PFAIL = .FALSE.
      CALL CHECK1('F06BEF',N,NENTER,ENTER,NCORR,CORR,TOL,.TRUE.,.FALSE.,
     +            PFAIL,'B')
      CORR(2,1) = 6
      CORR(2,3) = ONE
      CORR(3,1) = -124
      CORR(3,3) = 81
      CORR(5,1) = (TWO*EPS) + FIVE
      CORR(5,3) = -THREE
*     Perform (SROTJ) F06BEF tests with JOB = 'M'.
      CALL CHECK1('F06BEF',N,NENTER,ENTER,NCORR,CORR,TOL,.FALSE.,
     +            .FALSE.,PFAIL,'M')
      CORR(4,1) = 22
      CORR(4,3) = -180
*     Perform (SROTJ) F06BEF tests with JOB = 'S'.
      CALL CHECK1('F06BEF',N,NENTER,ENTER,NCORR,CORR,TOL,.FALSE.,.TRUE.,
     +            PFAIL,'S')
*
      STOP
*
      END
      SUBROUTINE CHECK0(FUNNAM,N,X,Y,Z,FTRUE,FAILTR,TOL)
*     .. Parameters ..
      INTEGER           NOUT
      PARAMETER         (NOUT=6)
*     .. Scalar Arguments ..
      DOUBLE PRECISION  TOL
      INTEGER           N
      CHARACTER*6       FUNNAM
*     .. Array Arguments ..
      DOUBLE PRECISION  FTRUE(N), X(N), Y(N), Z(N)
      LOGICAL           FAILTR(N)
*     .. Local Scalars ..
      DOUBLE PRECISION  F
      INTEGER           I
      LOGICAL           FAIL, FIRST, MISSED
*     .. External Functions ..
      DOUBLE PRECISION  F06BLF, F06BMF, F06BNF, F06BPF
      LOGICAL           NODIFF
      EXTERNAL          F06BLF, F06BMF, F06BNF, F06BPF, NODIFF
*     .. Executable Statements ..
      FIRST = .TRUE.
      WRITE (NOUT,*) ' Testing routine   ', FUNNAM
      DO 20 I = 1, N
         IF (FUNNAM.EQ.'F06BLF') THEN
            F = F06BLF(X(I),Y(I),FAIL)
         ELSE IF (FUNNAM.EQ.'F06BMF') THEN
            F = F06BMF(X(I),Y(I))
         ELSE IF (FUNNAM.EQ.'F06BPF') THEN
            F = F06BPF(X(I),Y(I),Z(I))
         ELSE IF (FUNNAM.EQ.'F06BNF') THEN
            F = F06BNF(X(I),Y(I))
         ELSE
            WRITE (NOUT,*) ' Shouldn''t be here in CHECK0'
            STOP
         END IF
         MISSED = .NOT. NODIFF(F,FTRUE(I),TOL)
         IF (FUNNAM.EQ.'F06BLF') MISSED = MISSED .OR. FAIL .NEQV.
     +       FAILTR(I)
         IF (MISSED) THEN
            IF (FIRST) THEN
               FIRST = .FALSE.
               WRITE (NOUT,*) '               **** FAIL ****'
            END IF
*           Give details of failure here.
            IF (FUNNAM.EQ.'F06BPF') THEN
               WRITE (NOUT,*) ' entered with ', X(I), Y(I), Z(I),
     +           ','
               WRITE (NOUT,*) ' returned ', F, ' instead of ',
     +           FTRUE(I)
            ELSE
               WRITE (NOUT,*) ' entered with ', X(I), Y(I), ','
               WRITE (NOUT,*) ' returned ', F, ' instead of ',
     +           FTRUE(I)
            END IF
         END IF
   20 CONTINUE
      IF (FIRST) THEN
         WRITE (NOUT,*) '               ---- PASS ----'
      END IF
      WRITE (NOUT,*)
*
      RETURN
      END
      SUBROUTINE CHECK1(FUNNAM,N,NENTER,ENTER,NCORR,CORR,TOL,HEAD,LAST,
     +                  PFAIL,JOB)
*     .. Parameters ..
      INTEGER           NOUT
      PARAMETER         (NOUT=6)
*     .. Scalar Arguments ..
      DOUBLE PRECISION  TOL
      INTEGER           N, NCORR, NENTER
      LOGICAL           HEAD, LAST, PFAIL
      CHARACTER         JOB
      CHARACTER*6       FUNNAM
*     .. Array Arguments ..
      DOUBLE PRECISION  CORR(6,6), ENTER(6,6)
*     .. Local Scalars ..
      DOUBLE PRECISION  EPSROT, RRTTWO
      INTEGER           I, K
      LOGICAL           MISSED
*     .. Local Arrays ..
      DOUBLE PRECISION  CPYENT(6), EXIT(6), FCORR(6)
*     .. External Functions ..
      DOUBLE PRECISION  X02AJF
      LOGICAL           NODIFV
      EXTERNAL          X02AJF, NODIFV
*     .. External Subroutines ..
      EXTERNAL          F06BAF, F06BCF, F06BEF, F06BHF
*     .. Intrinsic Functions ..
      INTRINSIC         ABS, SQRT
*     .. Executable Statements ..
      IF (HEAD) WRITE (NOUT,*) ' Testing routine   ', FUNNAM
      EPSROT = 2*X02AJF()
      DO 80 I = 1, N
         DO 20 K = 1, NENTER
            CPYENT(K) = ENTER(I,K)
   20    CONTINUE
         IF (FUNNAM.EQ.'F06BCF') THEN
            CALL F06BCF(ENTER(I,1),EXIT(1),EXIT(2))
         ELSE IF (FUNNAM.EQ.'F06BHF') THEN
            CALL F06BHF(ENTER(I,1),ENTER(I,2),ENTER(I,3),ENTER(I,4),
     +                  ENTER(I,5))
            EXIT(1) = ENTER(I,1)
            EXIT(2) = ENTER(I,2)
            EXIT(3) = ENTER(I,3)
         ELSE IF (FUNNAM.EQ.'F06BAF') THEN
            CALL F06BAF(ENTER(I,1),ENTER(I,2),EXIT(3),EXIT(4))
            EXIT(1) = ENTER(I,1)
            EXIT(2) = ENTER(I,2)
         ELSE IF (FUNNAM.EQ.'F06BEF') THEN
            CALL F06BEF(JOB,ENTER(I,1),ENTER(I,2),ENTER(I,3),EXIT(4),
     +                  EXIT(5))
            EXIT(1) = ENTER(I,1)
            EXIT(2) = ENTER(I,2)
            EXIT(3) = ENTER(I,3)
            CORR(I,2) = EXIT(2)
*           Not bothered about CORR (2) so set same as EXIT (2)
         ELSE
            WRITE (NOUT,*) ' Shouldn''t be here in CHECK1'
            STOP
         END IF
         DO 40 K = 1, NCORR
            FCORR(K) = CORR(I,K)
   40    CONTINUE
         MISSED = .NOT. NODIFV(NCORR,EXIT,FCORR,TOL)
         IF (FUNNAM.EQ.'F06BEF') THEN
            RRTTWO = 1
            RRTTWO = RRTTWO/SQRT(RRTTWO*2)
            IF (JOB.EQ.'B' .OR. JOB.EQ.'b') THEN
               IF (EXIT(4).LT.RRTTWO-EPSROT) MISSED = .TRUE.
            ELSE IF (JOB.EQ.'S' .OR. JOB.EQ.'s') THEN
               IF (EXIT(4).LT.0 .OR. EXIT(4).GT.RRTTWO+EPSROT)
     +             MISSED = .TRUE.
            ELSE IF (JOB.EQ.'M' .OR. JOB.EQ.'m') THEN
               IF (ABS(EXIT(1)).LT.ABS(EXIT(2))) MISSED = .TRUE.
            END IF
         END IF
         IF (MISSED) THEN
            IF ( .NOT. PFAIL) THEN
               PFAIL = .TRUE.
               WRITE (NOUT,*) '               **** FAIL ****'
            END IF
*           Give details of failure here.
            WRITE (NOUT,*) ' entered with ', (CPYENT(K),K=1,NENTER)
            IF (FUNNAM.EQ.'F06BEF') WRITE (NOUT,*) ' and JOB = ',
     +          JOB
            WRITE (NOUT,*) 'returned with ', (EXIT(K),K=1,NCORR),
     +        ','
            WRITE (NOUT,*) '   instead of ', (FCORR(K),K=1,NCORR)
         END IF
*
*        Restore entry values.
         DO 60 K = 1, NENTER
            ENTER(I,K) = CPYENT(K)
   60    CONTINUE
*
   80 CONTINUE
      IF (LAST .AND. .NOT. PFAIL) THEN
         WRITE (NOUT,*) '               ---- PASS ----'
      END IF
      IF (LAST) WRITE (NOUT,*)
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
      LOGICAL FUNCTION NODIFV(LEN,SCOMP,STRUE,TOL)
*     Returns .TRUE. if there is no difference between arrays SCOMP and
*     STRUE, to tolerance TOL. Differences are checked componentwise.
*     .. Scalar Arguments ..
      DOUBLE PRECISION        TOL
      INTEGER                 LEN
*     .. Array Arguments ..
      DOUBLE PRECISION        SCOMP(*), STRUE(*)
*     .. Local Scalars ..
      INTEGER                 I
*     .. External Functions ..
      LOGICAL                 NODIFF
      EXTERNAL                NODIFF
*     .. Executable Statements ..
      NODIFV = .TRUE.
      DO 20 I = 1, LEN
         NODIFV = NODIFV .AND. NODIFF(SCOMP(I),STRUE(I),TOL)
   20 CONTINUE
      RETURN
      END
