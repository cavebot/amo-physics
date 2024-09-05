      SUBROUTINE D01ATV(F,A,B,EPSABS,EPSREL,ALIST,BLIST,ELIST,RLIST,
     *                  LIMIT,IORD,LIORD,RESULT,ABSERR,NEVAL,IER)
C     MARK 13 RELEASE.  NAG COPYRIGHT 1988.
C     MARK 13B REVISED. IER-653 (AUG 1988).
C     BASED ON QUADPACK ROUTINE  QAGS.
C     ..................................................................
C
C        PURPOSE
C           THE ROUTINE CALCULATES AN APPROXIMATION  RESULT  TO A GIVEN
C           DEFINITE INTEGRAL   I = INTEGRAL OF  F  OVER (A,B),
C           HOPEFULLY SATISFYING FOLLOWING CLAIM FOR ACCURACY
C           ABS(I-RESULT).LE.MAX(EPSABS,EPSREL*ABS(I)).
C
C        PARAMETERS
C         ON ENTRY
C            F      - SUBROUTINE, SUPPLIED BY THE USER.
C
C                     F  MUST RETURN THE VALUE OF THE INTEGRAND AT
C                     A SET OF POINTS X(1),X(2),...,X(N). THAT IS,
C                     F(X(1)),F(X(2)),...,F(X(N)).
C
C                     ITS SPECIFICATION IS:
C                     SUBROUTINE F(X,FV,N)
C                     INTEGER    N
C                     REAL       X(N), FV(N)
C
C                     ON EXIT, FV(J) MUST BE SET TO THE VALUE OF THE
C                     INTEGRAND AT THE POINT X(J) FOR J = 1,2,...,N.
C                     THAT IS, FV(J) = F(X(J)). THE ACTUAL NAME FOR F
C                     NEEDS TO BE DECLARED  E X T E R N A L  IN THE
C                     DRIVER PROGRAM.
C
C            A      - REAL
C                     LOWER LIMIT OF INTEGRATION
C
C            B      - REAL
C                     UPPER LIMIT OF INTEGRATION
C
C            EPSABS - REAL
C                     ABSOLUTE ACCURACY REQUESTED
C            EPSREL - REAL
C                     RELATIVE ACCURACY REQUESTED
C
C            ALIST,BLIST,ELIST,RLIST
C                   - REAL WORK ARRAYS (FUNCTIONS DESCRIBED BELOW)
C
C            LIMIT  - INTEGER
C                     GIVES AN UPPERBOUND ON THE NUMBER OF SUBINTERVALS
C                     IN THE PARTITION OF (A,B), LIMIT.GE.1.
C
C            IORD   - INTEGER
C                     WORK ARRAY OF DIMENSION LIORD
C
C            LIORD  - INTEGER
C                     LENGTH OF IORD (=LIMIT)
C
C         ON RETURN
C            RESULT - REAL
C                     APPROXIMATION TO THE INTEGRAL
C
C            ABSERR - REAL
C                     ESTIMATE OF THE MODULUS OF THE ABSOLUTE ERROR,
C                     WHICH SHOULD EQUAL OR EXCEED ABS(I-RESULT)
C
C            NEVAL  - INTEGER
C                     NUMBER OF INTEGRAND EVALUATIONS
C
C            IER    - INTEGER
C                     IER = 0 NORMAL AND RELIABLE TERMINATION OF THE
C                             ROUTINE. IT IS ASSUMED THAT THE REQUESTED
C                             ACCURACY HAS BEEN ACHIEVED.
C                     IER.GT.0 ABNORMAL TERMINATION OF THE ROUTINE
C                             THE ESTIMATES FOR INTEGRAL AND ERROR ARE
C                             LESS RELIABLE. IT IS ASSUMED THAT THE
C                             REQUESTED ACCURACY HAS NOT BEEN ACHIEVED.
C                         = 1 MAXIMUM NUMBER OF SUBDIVISIONS ALLOWED
C                             HAS BEEN ACHIEVED. ONE CAN ALLOW MORE SUB-
C                             DIVISIONS BY INCREASING THE DATA VALUE OF
C                             LIMIT IN D01ATV (AND TAKING THE ACCORDING
C                             DIMENSION ADJUSTMENTS INTO ACCOUNT).
C                             HOWEVER, IF THIS YIELDS NO IMPROVEMENT
C                             IT IS ADVISED TO ANALYZE THE INTEGRAND
C                             IN ORDER TO DETERMINE THE INTEGRATION
C                             DIFFICULTIES. IF THE POSITION OF A
C                             LOCAL DIFFICULTY CAN BE DETERMINED (E.G.
C                             SINGULARITY, DISCONTINUITY WITHIN THE
C                             INTERVAL) ONE WILL PROBABLY GAIN FROM
C                             SPLITTING UP THE INTERVAL AT THIS POINT
C                             AND CALLING THE INTEGRATOR ON THE SUB-
C                             RANGES. IF POSSIBLE, AN APPROPRIATE
C                             SPECIAL-PURPOSE INTEGRATOR SHOULD BE USED,
C                             WHICH IS DESIGNED FOR HANDLING THE TYPE
C                             OF DIFFICULTY INVOLVED.
C                         = 2 THE OCCURRENCE OF ROUNDOFF ERROR IS DETEC-
C                             TED, WHICH PREVENTS THE REQUESTED
C                             TOLERANCE FROM BEING ACHIEVED.
C                             THE ERROR MAY BE UNDER-ESTIMATED.
C                         = 3 EXTREMELY BAD INTEGRAND BEHAVIOUR OCCURS
C                             AT SOME  POINTS OF THE INTEGRATION
C                             INTERVAL.
C                         = 4 THE ALGORITHM DOES NOT CONVERGE. ROUNDOFF
C                             ERROR IS DETECTED IN THE EXTRAPOLATION
C                             TABLE. IT IS PRESUMED THAT THE REQUESTED
C                             TOLERANCE CANNOT BE ACHIEVED, AND THAT THE
C                             RETURNED RESULT IS THE BEST WHICH CAN BE
C                             OBTAINED.
C                         = 5 THE INTEGRAL IS PROBABLY DIVERGENT, OR
C                             SLOWLY CONVERGENT. IT MUST BE NOTED THAT
C                             DIVERGENCE CAN OCCUR WITH ANY OTHER VALUE
C                             OF IER.
C     ..................................................................
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION  A, ABSERR, B, EPSABS, EPSREL, RESULT
      INTEGER           IER, LIMIT, LIORD, NEVAL
C     .. Array Arguments ..
      DOUBLE PRECISION  ALIST(LIMIT), BLIST(LIMIT), ELIST(LIMIT),
     *                  RLIST(LIMIT)
      INTEGER           IORD(LIORD)
C     .. Subroutine Arguments ..
      EXTERNAL          F
C     .. Local Scalars ..
      DOUBLE PRECISION  A1, A2, ABSEPS, AREA, AREA1, AREA12, AREA2, B1,
     *                  B2, CORREC, DEFAB1, DEFAB2, DEFABS, DRES,
     *                  EPMACH, ERLARG, ERLAST, ERRBND, ERRMAX, ERRO12,
     *                  ERROR1, ERROR2, ERRSUM, ERTEST, OFLOW, RESABS,
     *                  RESEPS, SMALL, UFLOW
      INTEGER           ID, IERRO, IERS, IROFF1, IROFF2, IROFF3, JUPBND,
     *                  K, KSGN, KTMIN, LAST, MAXERR, NERR, NRES, NRMAX,
     *                  NUMRL2
      LOGICAL           EXTRAP, NOEXT
C     .. Local Arrays ..
      DOUBLE PRECISION  RES3LA(3), RLIST2(52)
      CHARACTER*80      REC(2)
C     .. External Functions ..
      DOUBLE PRECISION  X02AJF, X02AMF
      EXTERNAL          X02AJF, X02AMF
C     .. External Subroutines ..
      EXTERNAL          D01AJX, D01AJY, D01ATZ, X04AAF, X04BAF
C     .. Intrinsic Functions ..
      INTRINSIC         ABS, MAX
C     .. Executable Statements ..
C
C            THE DIMENSION OF RLIST2 IS DETERMINED BY THE VALUE OF
C            LIMEXP IN SUBROUTINE D01AJY (RLIST2 SHOULD BE OF DIMENSION
C            (LIMEXP+2) AT LEAST).
C
C            LIST OF MAJOR VARIABLES
C            -----------------------
C
C           ALIST     - LIST OF LEFT END POINTS OF ALL SUBINTERVALS
C                       CONSIDERED UP TO NOW
C           BLIST     - LIST OF RIGHT END POINTS OF ALL SUBINTERVALS
C                       CONSIDERED UP TO NOW
C           RLIST(I)  - APPROXIMATION TO THE INTEGRAL OVER
C                       (ALIST(I),BLIST(I))
C           RLIST2    - ARRAY OF DIMENSION AT LEAST LIMEXP+2 CONTAINING
C                       THE PART OF THE EPSILON TABLE WHICH IS STILL
C                       NEEDED FOR FURTHER COMPUTATIONS
C           ELIST(I)  - ERROR ESTIMATE APPLYING TO RLIST(I)
C           MAXERR    - POINTER TO THE INTERVAL WITH LARGEST ERROR
C                       ESTIMATE
C           ERRMAX    - ELIST(MAXERR)
C           ERLAST    - ERROR ON THE INTERVAL CURRENTLY SUBDIVIDED
C                       (BEFORE THAT SUBDIVISION HAS TAKEN PLACE)
C           AREA      - SUM OF THE INTEGRALS OVER THE SUBINTERVALS
C           ERRSUM    - SUM OF THE ERRORS OVER THE SUBINTERVALS
C           ERRBND    - REQUESTED ACCURACY MAX(EPSABS,EPSREL*
C                       ABS(RESULT))
C           *****1    - VARIABLE FOR THE LEFT INTERVAL
C           *****2    - VARIABLE FOR THE RIGHT INTERVAL
C           LAST      - INDEX FOR SUBDIVISION
C           NRES      - NUMBER OF CALLS TO THE EXTRAPOLATION ROUTINE
C           NUMRL2    - NUMBER OF ELEMENTS CURRENTLY IN RLIST2. IF AN
C                       APPROPRIATE APPROXIMATION TO THE COMPOUNDED
C                       INTEGRAL HAS BEEN OBTAINED IT IS PUT IN
C                       RLIST2(NUMRL2) AFTER NUMRL2 HAS BEEN INCREASED
C                       BY ONE.
C           SMALL     - LENGTH OF THE SMALLEST INTERVAL CONSIDERED
C                       UP TO NOW, MULTIPLIED BY 1.5
C           ERLARG    - SUM OF THE ERRORS OVER THE INTERVALS LARGER
C                       THAN THE SMALLEST INTERVAL CONSIDERED UP TO NOW
C           EXTRAP    - LOGICAL VARIABLE DENOTING THAT THE ROUTINE IS
C                       ATTEMPTING TO PERFORM EXTRAPOLATION I.E. BEFORE
C                       SUBDIVIDING THE SMALLEST INTERVAL WE TRY TO
C                       DECREASE THE VALUE OF ERLARG.
C           NOEXT     - LOGICAL VARIABLE DENOTING THAT EXTRAPOLATION
C                       IS NO LONGER ALLOWED (TRUE VALUE)
C
      EPMACH = X02AJF()
      UFLOW = X02AMF()
      OFLOW = 1.0D+00/UFLOW
C
C            TEST ON VALIDITY OF PARAMETERS
C            ------------------------------
      IERS = IER
      IER = 0
      NEVAL = 0
      LAST = 0
      RESULT = 0.0D+00
      ABSERR = 0.0D+00
      ALIST(1) = A
      BLIST(1) = B
      RLIST(1) = 0.0D+00
      ELIST(1) = 0.0D+00
C
C           FIRST APPROXIMATION TO THE INTEGRAL
C           -----------------------------------
C
      IERRO = 0
      CALL D01ATZ(F,A,B,RESULT,ABSERR,DEFABS,RESABS)
C
C           TEST ON ACCURACY.
C
      DRES = ABS(RESULT)
      ERRBND = MAX(EPSABS,EPSREL*DRES)
      LAST = 1
      RLIST(1) = RESULT
      ELIST(1) = ABSERR
      IORD(1) = 1
      IF ((ABSERR.LE.ERRBND .AND. ABSERR.NE.RESABS)
     *    .OR. ABSERR.EQ.0.0D+00) GO TO 340
      IF (LIMIT.EQ.1) IER = 1
      IF (ABSERR.LE.1.0D+02*EPMACH*DEFABS .AND. ABSERR.GT.ERRBND)
     *    IER = 2
      IF (IER.NE.0) GO TO 340
C
C           INITIALIZATION
C           --------------
C
      RLIST2(1) = RESULT
      ERRMAX = ABSERR
      MAXERR = 1
      AREA = RESULT
      ERRSUM = ABSERR
      ABSERR = OFLOW
      NRMAX = 1
      NRES = 0
      NUMRL2 = 2
      KTMIN = 0
      EXTRAP = .FALSE.
      NOEXT = .FALSE.
      IROFF1 = 0
      IROFF2 = 0
      IROFF3 = 0
      KSGN = -1
      IF (DRES.GE.(1.0D+00-5.0D+01*EPMACH)*DEFABS) KSGN = 1
C
C           MAIN DO-LOOP
C           ------------
C
      DO 200 LAST = 2, LIMIT
C
C           BISECT THE SUBINTERVAL WITH THE NRMAX-TH LARGEST ERROR
C           ESTIMATE.
C
         A1 = ALIST(MAXERR)
         B1 = 5.0D-01*(ALIST(MAXERR)+BLIST(MAXERR))
         A2 = B1
         B2 = BLIST(MAXERR)
         ERLAST = ERRMAX
         CALL D01ATZ(F,A1,B1,AREA1,ERROR1,RESABS,DEFAB1)
         CALL D01ATZ(F,A2,B2,AREA2,ERROR2,RESABS,DEFAB2)
C
C           IMPROVE PREVIOUS APPROXIMATIONS TO INTEGRAL AND ERROR
C           AND TEST FOR ACCURACY.
C
         AREA12 = AREA1 + AREA2
         ERRO12 = ERROR1 + ERROR2
         ERRSUM = ERRSUM + ERRO12 - ERRMAX
         AREA = AREA + AREA12 - RLIST(MAXERR)
         IF (DEFAB1.EQ.ERROR1 .OR. DEFAB2.EQ.ERROR2) GO TO 40
         IF (ABS(RLIST(MAXERR)-AREA12).GT.1.0D-05*ABS(AREA12)
     *       .OR. ERRO12.LT.9.9D-01*ERRMAX) GO TO 20
         IF (EXTRAP) IROFF2 = IROFF2 + 1
         IF ( .NOT. EXTRAP) IROFF1 = IROFF1 + 1
   20    IF (LAST.GT.10 .AND. ERRO12.GT.ERRMAX) IROFF3 = IROFF3 + 1
   40    RLIST(MAXERR) = AREA1
         RLIST(LAST) = AREA2
         ERRBND = MAX(EPSABS,EPSREL*ABS(AREA))
C
C           APPEND THE NEWLY-CREATED INTERVALS TO THE LIST.
C
         IF (ERROR2.GT.ERROR1) GO TO 60
         ALIST(LAST) = A2
         BLIST(MAXERR) = B1
         BLIST(LAST) = B2
         ELIST(MAXERR) = ERROR1
         ELIST(LAST) = ERROR2
         GO TO 80
   60    ALIST(MAXERR) = A2
         ALIST(LAST) = A1
         BLIST(LAST) = B1
         RLIST(MAXERR) = AREA2
         RLIST(LAST) = AREA1
         ELIST(MAXERR) = ERROR2
         ELIST(LAST) = ERROR1
C
C           CALL SUBROUTINE D01AJX TO MAINTAIN THE DESCENDING ORDERING
C           IN THE LIST OF ERROR ESTIMATES AND SELECT THE SUBINTERVAL
C           WITH NRMAX-TH LARGEST ERROR ESTIMATE (TO BE BISECTED NEXT).
C
   80    CALL D01AJX(LIMIT,LAST,MAXERR,ERRMAX,ELIST,IORD,NRMAX)
C        ***JUMP OUT OF DO-LOOP
         IF (ERRSUM.LE.ERRBND) GO TO 280
C
C           SET ERROR FLAG IN THE CASE THAT THE NUMBER OF SUBINTERVALS
C           EQUALS LIMIT.
C
         IF (LAST.EQ.LIMIT) IER = 1
C
C           TEST FOR ROUNDOFF ERROR AND EVENTUALLY SET ERROR FLAG.
C
         IF (IROFF1+IROFF2.GE.10 .OR. IROFF3.GE.20) IER = 2
         IF (IROFF2.GE.5) IERRO = 3
C
C           SET ERROR FLAG IN THE CASE OF BAD INTEGRAND BEHAVIOUR
C           AT A POINT OF THE INTEGRATION RANGE.
C
         IF (MAX(ABS(A1),ABS(B2)).LE.(1.0D+00+1.0D+03*EPMACH)*(ABS(A2)
     *       +1.0D+03*UFLOW)) IER = 4
C        ***JUMP OUT OF DO-LOOP
         IF (IER.GT.1) GO TO 220
         IF (LAST.EQ.2) GO TO 180
         IF (NOEXT) GO TO 200
         ERLARG = ERLARG - ERLAST
         IF (ABS(B1-A1).GT.SMALL) ERLARG = ERLARG + ERRO12
         IF (EXTRAP) GO TO 100
C
C           TEST WHETHER THE INTERVAL TO BE BISECTED NEXT IS THE
C           SMALLEST INTERVAL.
C
         IF (ABS(BLIST(MAXERR)-ALIST(MAXERR)).GT.SMALL) GO TO 200
         EXTRAP = .TRUE.
         NRMAX = 2
  100    IF (IERRO.EQ.3 .OR. ERLARG.LE.ERTEST) GO TO 140
C
C           THE SMALLEST INTERVAL HAS THE LARGEST ERROR.
C           BEFORE BISECTING DECREASE THE SUM OF THE ERRORS OVER THE
C           LARGER INTERVALS (ERLARG) AND PERFORM EXTRAPOLATION.
C
         ID = NRMAX
         JUPBND = LAST
         IF (LAST.GT.(2+LIMIT/2)) JUPBND = LIMIT + 3 - LAST
         DO 120 K = ID, JUPBND
            MAXERR = IORD(NRMAX)
            ERRMAX = ELIST(MAXERR)
C           ***JUMP OUT OF DO-LOOP
            IF (ABS(BLIST(MAXERR)-ALIST(MAXERR)).GT.SMALL) GO TO 200
            NRMAX = NRMAX + 1
  120    CONTINUE
C
C           PERFORM EXTRAPOLATION.
C
  140    NUMRL2 = NUMRL2 + 1
         RLIST2(NUMRL2) = AREA
         CALL D01AJY(NUMRL2,RLIST2,RESEPS,ABSEPS,RES3LA,NRES)
         KTMIN = KTMIN + 1
         IF (KTMIN.GT.5 .AND. ABSERR.LT.1.0D-03*ERRSUM) IER = 5
         IF (ABSEPS.GE.ABSERR) GO TO 160
         KTMIN = 0
         ABSERR = ABSEPS
         RESULT = RESEPS
         CORREC = ERLARG
         ERTEST = MAX(EPSABS,EPSREL*ABS(RESEPS))
C        ***JUMP OUT OF DO-LOOP
         IF (ABSERR.LE.ERTEST) THEN
            IF (IER.EQ.1) IER = 0
            GO TO 220
         END IF
C
C           PREPARE BISECTION OF THE SMALLEST INTERVAL.
C
  160    IF (NUMRL2.EQ.1) NOEXT = .TRUE.
         IF (IER.EQ.5 .OR. IER.EQ.1) GO TO 220
         MAXERR = IORD(1)
         ERRMAX = ELIST(MAXERR)
         NRMAX = 1
         EXTRAP = .FALSE.
         SMALL = SMALL*5.0D-01
         ERLARG = ERRSUM
         GO TO 200
  180    SMALL = ABS(B-A)*3.75D-01
         ERLARG = ERRSUM
         ERTEST = ERRBND
         RLIST2(2) = AREA
  200 CONTINUE
      LAST = LIMIT
C
C           SET FINAL RESULT AND ERROR ESTIMATE.
C           ------------------------------------
C
  220 IF (ABSERR.EQ.OFLOW) GO TO 280
      IF (IER+IERRO.EQ.0) GO TO 260
      IF (IERRO.EQ.3) ABSERR = ABSERR + CORREC
      IF (IER.EQ.0) IER = 3
      IF (RESULT.NE.0.0D+00 .AND. AREA.NE.0.0D+00) GO TO 240
      IF (ABSERR.GT.ERRSUM) GO TO 280
      IF (AREA.EQ.0.0D+00) GO TO 320
      GO TO 260
  240 IF (ABSERR/ABS(RESULT).GT.ERRSUM/ABS(AREA)) GO TO 280
C
C           TEST ON DIVERGENCE.
C
  260 IF (KSGN.EQ.(-1) .AND. MAX(ABS(RESULT),ABS(AREA))
     *    .LE.DEFABS*1.0D-02) GO TO 320
      IF (1.0D-02.GT.(RESULT/AREA) .OR. (RESULT/AREA)
     *    .GT.1.0D+02 .OR. ERRSUM.GT.ABS(AREA)) IER = 6
      GO TO 320
C
C           COMPUTE GLOBAL INTEGRAL SUM.
C
  280 RESULT = 0.0D+00
      DO 300 K = 1, LAST
         RESULT = RESULT + RLIST(K)
  300 CONTINUE
      ABSERR = ERRSUM
  320 IF (IER.GT.2) IER = IER - 1
  340 NEVAL = 42*LAST - 21
      IORD(1) = LAST
      IF (IER.EQ.3 .AND. IERS.NE.1) THEN
         CALL X04AAF(0,NERR)
         WRITE (REC,FMT=99999) A1, B2
         CALL X04BAF(NERR,REC(1))
         CALL X04BAF(NERR,REC(2))
      END IF
      RETURN
C
99999 FORMAT (' ** Extremely bad integrand behaviour occurs around the',
     *       ' subinterval',/'    (',1P,D15.7,' , ',1P,D15.7,' )')
      END
