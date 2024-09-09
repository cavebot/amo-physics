*     D02QZF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          NEQF, NEQG, LATOL, LRTOL, LRWORK, LIWORK
      PARAMETER        (NEQF=2,NEQG=0,LATOL=NEQF,LRTOL=NEQF,
     +                 LRWORK=23+23*NEQF+14*NEQG,LIWORK=21+4*NEQG)
      DOUBLE PRECISION TSTART, HMAX
      PARAMETER        (TSTART=0.0D0,HMAX=2.0D0)
*     .. Local Scalars ..
      DOUBLE PRECISION PI, T, TCRIT, TINC, TOUT, TWANT
      INTEGER          I, IFAIL, J, MAXSTP, NWANT
      LOGICAL          ALTERG, CRIT, ONESTP, ROOT, SOPHST, VECTOL
      CHARACTER*1      STATEF
*     .. Local Arrays ..
      DOUBLE PRECISION ATOL(LATOL), RTOL(LRTOL), RWORK(LRWORK), Y(NEQF),
     +                 YPWANT(NEQF), YWANT(NEQF)
      INTEGER          IWORK(LIWORK)
*     .. External Functions ..
      DOUBLE PRECISION D02QFZ, X01AAF
      EXTERNAL         D02QFZ, X01AAF
*     .. External Subroutines ..
      EXTERNAL         D02QFF, D02QWF, D02QZF, FTRY03
*     .. Intrinsic Functions ..
      INTRINSIC        DBLE
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D02QZF Example Program Results'
      PI = X01AAF(0.0D0)
      STATEF = 'S'
      VECTOL = .TRUE.
      DO 20 I = 1, NEQF
         ATOL(I) = 1.0D-8
         RTOL(I) = 1.0D-4
   20 CONTINUE
      ONESTP = .TRUE.
      CRIT = .TRUE.
      TINC = 0.0625D0*PI
      TCRIT = 8.0D0*TINC
      TOUT = TCRIT
      MAXSTP = 500
      T = TSTART
      TWANT = TSTART + TINC
      NWANT = NEQF
      Y(1) = 0.0D0
      Y(2) = 1.0D0
      WRITE (NOUT,*)
      WRITE (NOUT,*) '  T         Y(1)     Y(2)'
      WRITE (NOUT,99999) T, Y(1), Y(2)
      IFAIL = -1
*
      CALL D02QWF(STATEF,NEQF,VECTOL,ATOL,LATOL,RTOL,LRTOL,ONESTP,CRIT,
     +            TCRIT,HMAX,MAXSTP,NEQG,ALTERG,SOPHST,RWORK,LRWORK,
     +            IWORK,LIWORK,IFAIL)
*
      J = 1
   40 IFAIL = -1
*
      CALL D02QFF(FTRY03,NEQF,T,Y,TOUT,D02QFZ,NEQG,ROOT,RWORK,LRWORK,
     +            IWORK,LIWORK,IFAIL)
*
      IF (IFAIL.EQ.0) THEN
   60    IF (TWANT.LE.T) THEN
            IFAIL = 0
*
            CALL D02QZF(NEQF,TWANT,NWANT,YWANT,YPWANT,RWORK,LRWORK,
     +                  IWORK,LIWORK,IFAIL)
*
            WRITE (NOUT,99999) TWANT, YWANT(1), YWANT(2)
            J = J + 1
            TWANT = TSTART + DBLE(J)*TINC
            GO TO 60
         END IF
         IF (T.LT.TOUT) GO TO 40
      END IF
      STOP
*
99999 FORMAT (1X,F6.4,3X,2(F7.4,2X))
      END
*
      SUBROUTINE FTRY03(NEQF,T,Y,YP)
*     .. Scalar Arguments ..
      DOUBLE PRECISION  T
      INTEGER           NEQF
*     .. Array Arguments ..
      DOUBLE PRECISION  Y(NEQF), YP(NEQF)
*     .. Executable Statements ..
      YP(1) = Y(2)
      YP(2) = -Y(1)
      RETURN
      END
