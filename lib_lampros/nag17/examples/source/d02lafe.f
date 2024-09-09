*     D02LAF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          NEQ, LRWORK, NWANT
      PARAMETER        (NEQ=2,LRWORK=16+20*NEQ,NWANT=NEQ)
*     .. Local Scalars ..
      DOUBLE PRECISION ECC, H, HNEXT, HSTART, HUSED, T, TEND, TINC,
     +                 TNEXT, TOL, TSTART, Y1, Y2, YP1, YP2
      INTEGER          IFAIL, ITOL, K, MAXSTP, NATT, NFAIL, NSUCC
      LOGICAL          HIGH, ONESTP, START
*     .. Local Arrays ..
      DOUBLE PRECISION RWORK(LRWORK), THRES(NEQ), THRESP(NEQ), Y(NEQ),
     +                 YDP(NEQ), YP(NEQ), YPWANT(NWANT), YWANT(NWANT)
*     .. External Subroutines ..
      EXTERNAL         D02LAF, D02LXF, D02LYF, D02LZF, FCN2BD
*     .. Intrinsic Functions ..
      INTRINSIC        SQRT
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D02LAF Example Program Results'
      HIGH = .FALSE.
      ONESTP = .TRUE.
      TINC = 2.0D0
*
*     Initial conditions
*
      TSTART = 0.0D0
      ECC = 0.5D0
      Y1 = 1.0D0 - ECC
      Y2 = 0.0D0
      YP1 = 0.0D0
      YP2 = SQRT((1.0D0+ECC)/(1.0D0-ECC))
      TEND = 20.0D0
*
      DO 60 ITOL = 4, 5
         TOL = 10.0D0**(-ITOL)
         WRITE (NOUT,*)
         WRITE (NOUT,99999) 'Calculation with TOL = ', TOL
         WRITE (NOUT,*)
         WRITE (NOUT,*) '   T      Y(1)       Y(2)'
*
*        Call D02LXF with default THRES,THRESP,MAXSTP and H
*
         THRES(1) = 0.0D0
         THRESP(1) = 0.0D0
         H = 0.0D0
         MAXSTP = 0
         START = .TRUE.
         IFAIL = 0
*
         CALL D02LXF(NEQ,H,TOL,THRES,THRESP,MAXSTP,START,ONESTP,HIGH,
     +               RWORK,LRWORK,IFAIL)
*
*        Set initial values
*
         Y(1) = Y1
         Y(2) = Y2
         YP(1) = YP1
         YP(2) = YP2
         T = TSTART
         TNEXT = T + TINC
         WRITE (NOUT,99998) T, (Y(K),K=1,NEQ)
*
*        Loop point for onestep mode
*
   20    IFAIL = -1
*
         CALL D02LAF(FCN2BD,NEQ,T,TEND,Y,YP,YDP,RWORK,LRWORK,IFAIL)
*
         IF (IFAIL.GT.0) THEN
            WRITE (NOUT,*)
            WRITE (NOUT,99997) 'D02LAF returned with IFAIL = ', IFAIL,
     +        '  at T = ', T
            STOP
         END IF
*
*        Loop point for interpolation
*
   40    IF (TNEXT.LE.T) THEN
            IFAIL = 0
*
            CALL D02LZF(NEQ,T,Y,YP,NEQ,TNEXT,YWANT,YPWANT,RWORK,LRWORK,
     +                  IFAIL)
*
            WRITE (NOUT,99998) TNEXT, (YWANT(K),K=1,NEQ)
            TNEXT = TNEXT + TINC
            GO TO 40
         END IF
*
         IF (T.LT.TEND) GO TO 20
*
         IFAIL = 0
*
         CALL D02LYF(NEQ,HNEXT,HUSED,HSTART,NSUCC,NFAIL,NATT,THRES,
     +               THRESP,RWORK,LRWORK,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,99996) ' Number of successful steps = ', NSUCC
         WRITE (NOUT,99996) ' Number of   failed   steps = ', NFAIL
   60 CONTINUE
      STOP
*
99999 FORMAT (1X,A,1P,D9.1)
99998 FORMAT (1X,F5.1,2(2X,F9.5))
99997 FORMAT (1X,A,I2,A,1P,D10.3)
99996 FORMAT (1X,A,I5)
      END
*
      SUBROUTINE FCN2BD(NEQ,T,Y,YDP)
*
*     Derivatives for two body problem in  y'' = f(t,y)  form
*
*     .. Scalar Arguments ..
      DOUBLE PRECISION  T
      INTEGER           NEQ
*     .. Array Arguments ..
      DOUBLE PRECISION  Y(NEQ), YDP(NEQ)
*     .. Local Scalars ..
      DOUBLE PRECISION  R
*     .. Intrinsic Functions ..
      INTRINSIC         SQRT
*     .. Executable Statements ..
      R = SQRT(Y(1)**2+Y(2)**2)**3
      YDP(1) = -Y(1)/R
      YDP(2) = -Y(2)/R
      RETURN
      END
