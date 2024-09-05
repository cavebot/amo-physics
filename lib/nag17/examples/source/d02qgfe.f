*     D02QGF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          NEQF, NEQG, LATOL, LRTOL, LRWORK, LIWORK
      PARAMETER        (NEQF=3,NEQG=1,LATOL=1,LRTOL=1,
     +                 LRWORK=23+23*NEQF+14*NEQG,LIWORK=21+4*NEQG)
      DOUBLE PRECISION TSTART, HMAX
      PARAMETER        (TSTART=0.0D0,HMAX=2.0D0)
*     .. Local Scalars ..
      DOUBLE PRECISION GRVCM, PI, T, TCRIT, TINC, TOUT, TRVCM
      INTEGER          I, IFAIL, IREVCM, J, KGRVCM, MAXSTP, YPRVCM,
     +                 YRVCM
      LOGICAL          ALTERG, CRIT, ONESTP, ROOT, SOPHST, VECTOL
      CHARACTER*1      STATEF
*     .. Local Arrays ..
      DOUBLE PRECISION ATOL(LATOL), RTOL(LRTOL), RWORK(LRWORK), Y(NEQF)
      INTEGER          IWORK(LIWORK)
*     .. External Functions ..
      DOUBLE PRECISION X01AAF
      EXTERNAL         X01AAF
*     .. External Subroutines ..
      EXTERNAL         D02QGF, D02QWF
*     .. Intrinsic Functions ..
      INTRINSIC        COS, DBLE, TAN
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D02QGF Example Program Results'
      TCRIT = 10.0D0
      STATEF = 'S'
      VECTOL = .FALSE.
      RTOL(1) = 1.0D-4
      ATOL(1) = 1.0D-7
      ONESTP = .FALSE.
      SOPHST = .TRUE.
      CRIT = .TRUE.
      TINC = 2.0D0
      MAXSTP = 500
      PI = X01AAF(0.0D0)
      T = TSTART
      Y(1) = 0.5D0
      Y(2) = 0.5D0
      Y(3) = 0.2D0*PI
      WRITE (NOUT,*)
      WRITE (NOUT,*) '  T         Y(1)     Y(2)     Y(3)'
      WRITE (NOUT,99999) T, (Y(I),I=1,NEQF)
      IFAIL = 0
*
      CALL D02QWF(STATEF,NEQF,VECTOL,ATOL,LATOL,RTOL,LRTOL,ONESTP,CRIT,
     +            TCRIT,HMAX,MAXSTP,NEQG,ALTERG,SOPHST,RWORK,LRWORK,
     +            IWORK,LIWORK,IFAIL)
*
      J = 1
      TOUT = DBLE(J)*TINC
      IREVCM = 0
*
   20 IFAIL = -1
*
      CALL D02QGF(NEQF,T,Y,TOUT,NEQG,ROOT,IREVCM,TRVCM,YRVCM,YPRVCM,
     +            GRVCM,KGRVCM,RWORK,LRWORK,IWORK,LIWORK,IFAIL)
*
      IF (IREVCM.GT.0) THEN
         IF (IREVCM.LT.8) THEN
            IF (YRVCM.EQ.0) THEN
               RWORK(YPRVCM) = TAN(Y(3))
               RWORK(YPRVCM+1) = -0.032D0*TAN(Y(3))/Y(2) - 0.02D0*Y(2)
     +                           /COS(Y(3))
               RWORK(YPRVCM+2) = -0.032D0/Y(2)**2
            ELSE
               RWORK(YPRVCM) = TAN(RWORK(YRVCM+2))
               RWORK(YPRVCM+1) = -0.032D0*TAN(RWORK(YRVCM+2))
     +                           /RWORK(YRVCM+1) - 0.02D0*RWORK(YRVCM+1)
     +                           /COS(RWORK(YRVCM+2))
               RWORK(YPRVCM+2) = -0.032D0/RWORK(YRVCM+1)**2
            END IF
         ELSE IF (IREVCM.GT.8) THEN
            GRVCM = Y(1)
         END IF
         GO TO 20
      ELSE IF (IFAIL.EQ.0) THEN
         WRITE (NOUT,99999) T, (Y(I),I=1,NEQF)
         IF (T.EQ.TOUT .AND. J.LT.5) THEN
            J = J + 1
            TOUT = DBLE(J)*TINC
            GO TO 20
         END IF
      END IF
      STOP
*
99999 FORMAT (1X,F6.4,3X,3(F7.4,2X))
      END
