*     D02PZF Example Program Text
*     Mark 17 Revised.  NAG Copyright 1995.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          NEQ, LENWRK, METHOD
      PARAMETER        (NEQ=4,LENWRK=32*NEQ,METHOD=3)
      DOUBLE PRECISION ZERO, ONE, THREE, ECC
      PARAMETER        (ZERO=0.0D0,ONE=1.0D0,THREE=3.0D0,ECC=0.7D0)
*     .. Local Scalars ..
      DOUBLE PRECISION ERRMAX, HNEXT, HSTART, PI, TEND, TERRMX, TGOT,
     +                 TOL, TSTART, TWANT, WASTE
      INTEGER          IFAIL, L, STPCST, STPSOK, TOTF
      LOGICAL          ERRASS
*     .. Local Arrays ..
      DOUBLE PRECISION RMSERR(NEQ), THRES(NEQ), WORK(LENWRK), YGOT(NEQ),
     +                 YMAX(NEQ), YPGOT(NEQ), YSTART(NEQ)
*     .. External Functions ..
      DOUBLE PRECISION X01AAF
      EXTERNAL         X01AAF
*     .. External Subroutines ..
      EXTERNAL         D02PCF, D02PVF, D02PYF, D02PZF, F
*     .. Intrinsic Functions ..
      INTRINSIC        SQRT
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D02PZF Example Program Results'
*
*  Set initial conditions and input for D02PVF
*
      PI = X01AAF(ZERO)
      TSTART = ZERO
      YSTART(1) = ONE - ECC
      YSTART(2) = ZERO
      YSTART(3) = ZERO
      YSTART(4) = SQRT((ONE+ECC)/(ONE-ECC))
      TEND = THREE*PI
      DO 20 L = 1, NEQ
         THRES(L) = 1.0D-10
   20 CONTINUE
      ERRASS = .TRUE.
      HSTART = ZERO
      TOL = 1.0D-6
      IFAIL = 0
      CALL D02PVF(NEQ,TSTART,YSTART,TEND,TOL,THRES,METHOD,'Usual Task',
     +            ERRASS,HSTART,WORK,LENWRK,IFAIL)
*
      WRITE (NOUT,'(/A,D8.1)') ' Calculation with TOL = ', TOL
      WRITE (NOUT,'(/A/)') '    t         y1         y2'//
     +  '         y3         y4'
      WRITE (NOUT,'(1X,F6.3,4(3X,F8.4))') TSTART,
     +  (YSTART(L),L=1,NEQ)
*
      TWANT = TEND
*
   40 CONTINUE
      IFAIL = 1
      CALL D02PCF(F,TWANT,TGOT,YGOT,YPGOT,YMAX,WORK,IFAIL)
*
      IF (IFAIL.GE.2 .AND. IFAIL.LE.4) THEN
         GO TO 40
      ELSE IF (IFAIL.NE.0) THEN
         WRITE (NOUT,'(A,I2)') ' D02PCF returned with IFAIL set to',
     +     IFAIL
      ELSE
         WRITE (NOUT,'(1X,F6.3,4(3X,F8.4))') TGOT, (YGOT(L),L=1,NEQ)
*
         IFAIL = 0
         CALL D02PZF(RMSERR,ERRMAX,TERRMX,WORK,IFAIL)
         WRITE (NOUT,'(/A/9X,4(2X,E9.2))')
     +     ' Componentwise error '//'assessment', (RMSERR(L),L=1,NEQ)
         WRITE (NOUT,'(/A,E9.2,A,F6.3)')
     +     ' Worst global error observed '//'was ', ERRMAX,
     +     ' - it occurred at T = ', TERRMX
*
         IFAIL = 0
         CALL D02PYF(TOTF,STPCST,WASTE,STPSOK,HNEXT,IFAIL)
         WRITE (NOUT,'(/A,I6)')
     +     ' Cost of the integration in evaluations of F is', TOTF
      END IF
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
