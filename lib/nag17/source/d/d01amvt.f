      SUBROUTINE D01AMV(F,BOUND,INF,EPSABS,EPSREL,ALIST,BLIST,ELIST,
     *                  RLIST,LIMIT,IORD,LIORD,RESULT,ABSERR,NEVAL,IER)
C     MARK 13 RE-ISSUE. NAG COPYRIGHT 1988.
C     MARK 14 REVISED. IER-710 (DEC 1989).
C     BASED ON QUADPACK ROUTINE  QAGI.
C     ..................................................................
C
C        PURPOSE
C           THE ROUTINE CALCULATES AN APPROXIMATION  RESULT  TO A GIVEN
C           INTEGRAL    I = INTEGRAL OF  F  OVER (BOUND,+INFINITY)
C                    OR I = INTEGRAL OF  F  OVER (-INFINITY,BOUND)
C                    OR I = INTEGRAL OF  F  OVER (-INFINITY,+INFINITY),
C           HOPEFULLY SATISFYING FOLLOWING CLAIM FOR ACCURACY
C           ABS(I-RESULT).LE.MAX(EPSABS,EPSREL*ABS(I)).
C
C        PARAMETERS
C         ON ENTRY
C            F      - REAL
C                     FUNCTION SUBPROGRAM DEFINING THE INTEGRAND
C                     FUNCTION F(X). THE ACTUAL NAME FOR F NEEDS TO BE
C                     DECLARED E X T E R N A L IN THE DRIVER PROGRAM.
C
C            BOUND  - REAL
C                     FINITE BOUND OF INTEGRATION RANGE
C                     (HAS NO MEANING IF INTERVAL IS DOUBLY-INFINITE)
C
C            INF    - REAL
C                     INDICATING THE KIND OF INTEGRATION RANGE INVOLVED
C                     INF = 1 CORRESPONDS TO  (BOUND,+INFINITY),
C                     INF = -1            TO  (-INFINITY,BOUND),
C                     INF = 2             TO (-INFINITY,+INFINITY).
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
C                   - IER.GT.0 ABNORMAL TERMINATION OF THE ROUTINE. THE
C                             ESTIMATES FOR RESULT AND ERROR ARE LESS
C                             RELIABLE. IT IS ASSUMED THAT THE REQUESTED
C                             ACCURACY HAS NOT BEEN ACHIEVED.
C                     IER = 1 MAXIMUM NUMBER OF SUBDIVISIONS ALLOWED
C                             HAS BEEN ACHIEVED. ONE CAN ALLOW MORE
C                             SUBDIVISIONS BY INCREASING THE DATA VALUE
C                         OF LIMIT IN D01AMV (AND TAKING THE ACCORDING
C                             DIMENSION ADJUSTMENTS INTO ACCOUNT).
C                             HOWEVER, IF THIS YIELDS NO IMPROVEMENT
C                             IT IS ADVISED TO ANALYZE THE INTEGRAND
C                             IN ORDER TO DETERMINE THE INTEGRATION
C                             DIFFICULTIES. IF THE POSITION OF A LOCAL
C                             DIFFICULTY CAN BE DETERMINED (E.G.
C                             SINGULARITY, DISCONTINUITY WITHIN THE
C                             INTERVAL) ONE WILL PROBABLY GAIN FROM
C                             SPLITTING UP THE INTERVAL AT THIS POINT
C                             AND CALLING THE INTEGRATOR ON THE
C                             SUBRANGES. IF POSSIBLE, AN APPROPRIATE
C                             SPECIAL-PURPOSE INTEGRATOR SHOULD BE USED,
C                             WHICH IS DESIGNED FOR HANDLING THE TYPE
C                             OF DIFFICULTY INVOLVED.
C                         = 2 THE OCCURRENCE OF ROUNDOFF ERROR IS
C                             DETECTED, WHICH PREVENTS THE REQUESTED
C                             TOLERANCE FROM BEING ACHIEVED.
C                             THE ERROR MAY BE UNDER-ESTIMATED.
C                         = 3 EXTREMELY BAD INTEGRAND BEHAVIOUR OCCURS
C                             AT SOME POINTS OF THE INTEGRATION
C                             INTERVAL.
C                         = 4 THE ALGORITHM DOES NOT CONVERGE.
C                             ROUNDOFF ERROR IS DETECTED IN THE
C                             EXTRAPOLATION TABLE.
C                             IT IS ASSUMED THAT THE REQUESTED TOLERANCE
C                             CANNOT BE ACHIEVED, AND THAT THE RETURNED
C                             RESULT IS THE BEST WHICH CAN BE OBTAINED.
C                         = 5 THE INTEGRAL IS PROBABLY DIVERGENT, OR
C                             SLOWLY CONVERGENT. IT MUST BE NOTED THAT
C                             DIVERGENCE CAN OCCUR WITH ANY OTHER VALUE
C                             OF IER.
C
C     ..................................................................
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION  ABSERR, BOUND, EPSABS, EPSREL, RESULT
      INTEGER           IER, INF, LIMIT, LIORD, NEVAL
C     .. Array Arguments ..
      DOUBLE PRECISION  ALIST(LIMIT), BLIST(LIMIT), ELIST(LIMIT),
     *                  RLIST(LIMIT)
      INTEGER           IORD(LIMIT)
C     .. Function Arguments ..
      DOUBLE PRECISION  F
      EXTERNAL          F
C     .. Local Scalars ..
      DOUBLE PRECISION  A1, A2, ABSEPS, AREA, AREA1, AREA12, AREA2, B1,
     *                  B2, BOUN, CORREC, DEFAB1, DEFAB2, DEFABS, DRES,
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
      EXTERNAL          D01AJX, D01AJY, D01AMZ, X04AAF, X04BAF
C     .. Intrinsic Functions ..
      INTRINSIC         ABS, MAX
C     .. Executable Statements ..
C
C            THE DIMENSION OF RLIST2 IS DETERMINED BY THE VALUE OF
C            LIMEXP IN SUBROUTINE D01AJY.
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
C           RLIST2    - ARRAY OF DIMENSION AT LEAST (LIMEXP+2),
C                       CONTAINING THE PART OF THE EPSILON TABLE
C                       WHICH IS STILL NEEDED FOR FURTHER COMPUTATIONS
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
C           *****1    - VARIABLE FOR THE LEFT SUBINTERVAL
C           *****2    - VARIABLE FOR THE RIGHT SUBINTERVAL
C           LAST      - INDEX FOR SUBDIVISION
C           NRES      - NUMBER OF CALLS TO THE EXTRAPOLATION ROUTINE
C           NUMRL2    - NUMBER OF ELEMENTS CURRENTLY IN RLIST2. IF AN
C                       APPROPRIATE APPROXIMATION TO THE COMPOUNDED
C                       INTEGRAL HAS BEEN OBTAINED, IT IS PUT IN
C                       RLIST2(NUMRL2) AFTER NUMRL2 HAS BEEN INCREASED
C                       BY ONE.
C           SMALL     - LENGTH OF THE SMALLEST INTERVAL CONSIDERED UP
C                       TO NOW, MULTIPLIED BY 1.5
C           ERLARG    - SUM OF THE ERRORS OVER THE INTERVALS LARGER
C                       THAN THE SMALLEST INTERVAL CONSIDERED UP TO NOW
C           EXTRAP    - LOGICAL VARIABLE DENOTING THAT THE ROUTINE
C                       IS ATTEMPTING TO PERFORM EXTRAPOLATION. I.E.
C                       BEFORE SUBDIVIDING THE SMALLEST INTERVAL WE
C                       TRY TO DECREASE THE VALUE OF ERLARG.
C           NOEXT     - LOGICAL VARIABLE DENOTING THAT EXTRAPOLATION
C                       IS NO LONGER ALLOWED (TRUE-VALUE)
C
      EPMACH = X02AJF()
      UFLOW = X02AMF()
      OFLOW = 1.0D+00/UFLOW
C
C           TEST ON VALIDITY OF PARAMETERS
C           -----------------------------
C
      IERS = IER
      IER = 0
      NEVAL = 0
      LAST = 0
      RESULT = 0.0D+00
      ABSERR = 0.0D+00
      ALIST(1) = 0.0D+00
      BLIST(1) = 1.0D+00
      RLIST(1) = 0.0D+00
      ELIST(1) = 0.0D+00
      IORD(1) = 0
C
C           FIRST APPROXIMATION TO THE INTEGRAL
C           -----------------------------------
C
C           DETERMINE THE INTERVAL TO BE MAPPED ONTO (0,1).
C           IF INF = 2 THE INTEGRAL IS COMPUTED AS I = I1+I2, WHERE
C           I1 = INTEGRAL OF F OVER (-INFINITY,0),
C           I2 = INTEGRAL OF F OVER (0,+INFINITY).
C
      BOUN = BOUND
      IF (INF.EQ.2) BOUN = 0.0D+00
      CALL D01AMZ(F,BOUN,INF,0.0D+00,1.0D+00,RESULT,ABSERR,DEFABS,
     *            RESABS)
C
C           TEST ON ACCURACY
C
      LAST = 1
      RLIST(1) = RESULT
      ELIST(1) = ABSERR
      IORD(1) = 1
      DRES = ABS(RESULT)
      ERRBND = MAX(EPSABS,EPSREL*DRES)
      IF ((ABSERR.LE.ERRBND .AND. ABSERR.NE.RESABS)
     *    .OR. ABSERR.EQ.0.0D+00) GO TO 320
      IF (LIMIT.EQ.1) IER = 1
      IF (ABSERR.LE.1.0D+02*EPMACH*DEFABS .AND. ABSERR.GT.ERRBND)
     *    IER = 2
      IF (IER.NE.0) GO TO 320
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
      KTMIN = 0
      NUMRL2 = 2
      EXTRAP = .FALSE.
      NOEXT = .FALSE.
      IERRO = 0
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
C           BISECT THE SUBINTERVAL WITH NRMAX-TH LARGEST ERROR ESTIMATE.
C
         A1 = ALIST(MAXERR)
         B1 = 5.0D-01*(ALIST(MAXERR)+BLIST(MAXERR))
         A2 = B1
         B2 = BLIST(MAXERR)
         ERLAST = ERRMAX
         CALL D01AMZ(F,BOUN,INF,A1,B1,AREA1,ERROR1,RESABS,DEFAB1)
         CALL D01AMZ(F,BOUN,INF,A2,B2,AREA2,ERROR2,RESABS,DEFAB2)
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
C           AT SOME POINTS OF THE INTEGRATION RANGE.
C
         IF (MAX(ABS(A1),ABS(B2)).LE.(1.0D+00+1.0D+03*EPMACH)*(ABS(A2)
     *       +1.0D+03*UFLOW)) IER = 4
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
         IF (ABSERR.LE.ERTEST) THEN
            IF (IER.EQ.1) IER = 0
            GO TO 220
         END IF
C
C            PREPARE BISECTION OF THE SMALLEST INTERVAL.
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
  180    SMALL = 3.75D-01
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
      IF ((IER+IERRO).EQ.0) GO TO 260
      IF (IERRO.EQ.3) ABSERR = ABSERR + CORREC
      IF (IER.EQ.0) IER = 3
      IF (RESULT.NE.0.0D+00 .AND. AREA.NE.0.0D+00) GO TO 240
      IF (ABSERR.GT.ERRSUM) GO TO 280
      IF (AREA.EQ.0.0D+00) GO TO 320
      GO TO 260
  240 IF (ABSERR/ABS(RESULT).GT.ERRSUM/ABS(AREA)) GO TO 280
C
C           TEST ON DIVERGENCE
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
  320 NEVAL = 30*LAST - 15
      IORD(1) = LAST
      IF (INF.EQ.2) NEVAL = 2*NEVAL
      IF (IER.GT.2) IER = IER - 1
      IF (IER.EQ.3 .AND. IERS.NE.1) THEN
         CALL X04AAF(0,NERR)
         IF (INF.EQ.1) THEN
            A1 = BOUND + (1.0D+00-A1)/A1
            B2 = BOUND + (1.0D+00-B2)/B2
            IF (A1.GE.0.0D+00) THEN
               IF (B2.GE.0.0D+00) WRITE (REC,FMT=99999) B2, A1
            ELSE
               IF (B2.GE.0.0D+00) THEN
                  WRITE (REC,FMT=99999) A1, B2
               ELSE
                  WRITE (REC,FMT=99999) B2, A1
               END IF
            END IF
         ELSE IF (INF.EQ.-1) THEN
            A1 = BOUND - (1.0D+00-A1)/A1
            B2 = BOUND - (1.0D+00-B2)/B2
            IF (A1.GE.0.0D+00) THEN
               IF (B2.GE.0.0D+00) WRITE (REC,FMT=99999) A1, B2
            ELSE
               IF (B2.GE.0.0D+00) THEN
                  WRITE (REC,FMT=99999) B2, A1
               ELSE
                  WRITE (REC,FMT=99999) A1, B2
               END IF
            END IF
         ELSE
            A1 = (1.0D+00-A1)/A1
            B2 = (1.0D+00-B2)/B2
            IF (A1.GE.0.0D+00) THEN
               IF (B2.GE.0.0D+00) WRITE (REC,FMT=99998) B2, A1, -A1, -B2
            ELSE
               IF (B2.GE.0.0D+00) THEN
                  WRITE (REC,FMT=99998) A1, B2, -B2, -A1
               ELSE
                  WRITE (REC,FMT=99998) B2, A1, -A1, -B2
               END IF
            END IF
         END IF
         CALL X04BAF(NERR,REC(1))
         CALL X04BAF(NERR,REC(2))
      END IF
      RETURN
C
99999 FORMAT (' ** Extremely bad integrand behaviour occurs around the',
     *       ' subinterval',/'    (',1P,D15.7,' , ',1P,D15.7,' )')
99998 FORMAT (' ** Extremely bad integrand behaviour occurs around one',
     *       ' of the subintervals',/'    (',1P,D15.7,' , ',1P,D15.7,
     *       ' ) or (',1P,D15.7,' , ',1P,D15.7,' )')
      END
