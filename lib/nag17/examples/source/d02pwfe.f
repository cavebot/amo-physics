*     D02PWF Example Program Text
*     Mark 17 Revised.  NAG Copyright 1995.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          NEQ, LENWRK, METHOD
      PARAMETER        (NEQ=4,LENWRK=32*NEQ,METHOD=3)
      DOUBLE PRECISION ZERO, ONE, SIX, ECC
      PARAMETER        (ZERO=0.0D0,ONE=1.0D0,SIX=6.0D0,ECC=0.7D0)
*     .. Local Scalars ..
      DOUBLE PRECISION HNEXT, HSTART, PI, TEND, TFINAL, TINC, TNOW, TOL,
     +                 TSTART, WASTE
      INTEGER          I, IFAIL, J, L, NPTS, STPCST, STPSOK, TOTF
      LOGICAL          ERRASS
*     .. Local Arrays ..
      DOUBLE PRECISION THRES(NEQ), WORK(LENWRK), YNOW(NEQ), YPNOW(NEQ),
     +                 YSTART(NEQ)
*     .. External Functions ..
      DOUBLE PRECISION X01AAF
      EXTERNAL         X01AAF
*     .. External Subroutines ..
      EXTERNAL         D02PDF, D02PVF, D02PWF, D02PYF, F
*     .. Intrinsic Functions ..
      INTRINSIC        SQRT
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D02PWF Example Program Results'
*
*  Set initial conditions and input for D02PVF
*
      PI = X01AAF(ZERO)
      TSTART = ZERO
      YSTART(1) = ONE - ECC
      YSTART(2) = ZERO
      YSTART(3) = ZERO
      YSTART(4) = SQRT((ONE+ECC)/(ONE-ECC))
      TFINAL = SIX*PI
      DO 20 L = 1, NEQ
         THRES(L) = 1.0D-10
   20 CONTINUE
      ERRASS = .FALSE.
      HSTART = ZERO
*
*  Set output control
*
      NPTS = 6
      TINC = TFINAL/NPTS
*
      DO 60 I = 1, 2
         IF (I.EQ.1) TOL = 1.0D-4
         IF (I.EQ.2) TOL = 1.0D-5
         J = NPTS - 1
         TEND = TFINAL - J*TINC
         IFAIL = 0
         CALL D02PVF(NEQ,TSTART,YSTART,TEND,TOL,THRES,METHOD,
     +               'Complex Task',ERRASS,HSTART,WORK,LENWRK,IFAIL)
*
         WRITE (NOUT,'(/A,D8.1)') ' Calculation with TOL = ', TOL
         WRITE (NOUT,'(/A/)') '    t         y1         y2'//
     +     '         y3         y4'
         WRITE (NOUT,'(1X,F6.3,4(3X,F8.4))') TSTART,
     +     (YSTART(L),L=1,NEQ)
*
   40    CONTINUE
         IFAIL = -1
         CALL D02PDF(F,TNOW,YNOW,YPNOW,WORK,IFAIL)
*
         IF (IFAIL.EQ.0) THEN
            IF (TNOW.LT.TEND) GO TO 40
            WRITE (NOUT,'(1X,F6.3,4(3X,F8.4))') TNOW,
     +        (YNOW(L),L=1,NEQ)
            IF (TNOW.LT.TFINAL) THEN
               J = J - 1
               TEND = TFINAL - J*TINC
               CALL D02PWF(TEND,IFAIL)
               GO TO 40
            END IF
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
*     .. Local Scalars ..
      DOUBLE PRECISION R
*     .. Intrinsic Functions ..
      INTRINSIC    SQRT
*     .. Executable Statements ..
      R = SQRT(Y(1)**2+Y(2)**2)
      YP(1) = Y(3)
      YP(2) = Y(4)
      YP(3) = -Y(1)/R**3
      YP(4) = -Y(2)/R**3
      RETURN
      END
