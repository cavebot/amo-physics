*     D02PDF Example Program Text
*     Mark 17 Revised.  NAG Copyright 1995.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          NEQ, LENWRK, METHOD
      PARAMETER        (NEQ=2,LENWRK=32*NEQ,METHOD=2)
      DOUBLE PRECISION ZERO, ONE, TWO
      PARAMETER        (ZERO=0.0D0,ONE=1.0D0,TWO=2.0D0)
*     .. Local Scalars ..
      DOUBLE PRECISION HNEXT, HSTART, PI, TEND, TNOW, TOL, TSTART, WASTE
      INTEGER          I, IFAIL, L, STPCST, STPSOK, TOTF
      LOGICAL          ERRASS
*     .. Local Arrays ..
      DOUBLE PRECISION THRES(NEQ), WORK(LENWRK), YNOW(NEQ), YPNOW(NEQ),
     +                 YSTART(NEQ)
*     .. External Functions ..
      DOUBLE PRECISION X01AAF
      EXTERNAL         X01AAF
*     .. External Subroutines ..
      EXTERNAL         D02PDF, D02PVF, D02PYF, F
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D02PDF Example Program Results'
*
*  Set initial conditions and input for D02PVF
*
      PI = X01AAF(ZERO)
      TSTART = ZERO
      YSTART(1) = ZERO
      YSTART(2) = ONE
      TEND = TWO*PI
      DO 20 L = 1, NEQ
         THRES(L) = 1.0D-8
   20 CONTINUE
      ERRASS = .FALSE.
      HSTART = ZERO
*
      DO 60 I = 1, 2
         IF (I.EQ.1) TOL = 1.0D-4
         IF (I.EQ.2) TOL = 1.0D-5
         IFAIL = 0
         CALL D02PVF(NEQ,TSTART,YSTART,TEND,TOL,THRES,METHOD,
     +               'Complex Task',ERRASS,HSTART,WORK,LENWRK,IFAIL)
*
         WRITE (NOUT,'(/A,D8.1)') ' Calculation with TOL = ', TOL
         WRITE (NOUT,'(/A/)') '    t         y1         y2'
         WRITE (NOUT,'(1X,F6.3,2(3X,F8.4))') TSTART,
     +     (YSTART(L),L=1,NEQ)
*
   40    CONTINUE
         IFAIL = -1
         CALL D02PDF(F,TNOW,YNOW,YPNOW,WORK,IFAIL)
*
         IF (IFAIL.EQ.0) THEN
            WRITE (NOUT,'(1X,F6.3,2(3X,F8.4))') TNOW,
     +        (YNOW(L),L=1,NEQ)
            IF (TNOW.LT.TEND) GO TO 40
         END IF
*
         IFAIL = 0
         CALL D02PYF(TOTF,STPCST,WASTE,STPSOK,HNEXT,IFAIL)
         WRITE (NOUT,'(/A,I6)')
     +     ' Cost of the integration in evaluations of F is', TOTF
*
   60 CONTINUE
*
      STOP
      END
      SUBROUTINE F(T,Y,YP)
*     .. Scalar Arguments ..
      DOUBLE PRECISION T
*     .. Array Arguments ..
      DOUBLE PRECISION Y(*), YP(*)
*     .. Executable Statements ..
      YP(1) = Y(2)
      YP(2) = -Y(1)
      RETURN
      END
