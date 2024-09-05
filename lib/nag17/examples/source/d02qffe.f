*     D02QFF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          NEQF, NEQG, LATOL, LRTOL, LRWORK, LIWORK
      PARAMETER        (NEQF=2,NEQG=2,LATOL=NEQF,LRTOL=NEQF,
     +                 LRWORK=23+23*NEQF+14*NEQG,LIWORK=21+4*NEQG)
      DOUBLE PRECISION TSTART, HMAX
      PARAMETER        (TSTART=0.0D0,HMAX=0.0D0)
*     .. Local Scalars ..
      DOUBLE PRECISION HLAST, HNEXT, T, TCRIT, TCURR, TOLFAC, TOUT
      INTEGER          BADCMP, I, IFAIL, INDEX, MAXSTP, NFAIL, NSUCC,
     +                 ODLAST, ODNEXT, TYPE
      LOGICAL          ALTERG, CRIT, ONESTP, ROOT, SOPHST, VECTOL
      CHARACTER*1      STATEF
*     .. Local Arrays ..
      DOUBLE PRECISION ATOL(LATOL), RESIDS(NEQG), RTOL(LRTOL),
     +                 RWORK(LRWORK), Y(NEQF), YP(NEQF)
      INTEGER          EVENTS(NEQG), IWORK(LIWORK)
*     .. External Functions ..
      DOUBLE PRECISION GTRY02
      EXTERNAL         GTRY02
*     .. External Subroutines ..
      EXTERNAL         D02QFF, D02QWF, D02QXF, D02QYF, FTRY02
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D02QFF Example Program Results'
      TCRIT = 10.0D0
      STATEF = 'S'
      VECTOL = .TRUE.
      ONESTP = .FALSE.
      CRIT = .TRUE.
      MAXSTP = 0
      SOPHST = .TRUE.
      DO 20 I = 1, NEQF
         RTOL(I) = 1.0D-4
         ATOL(I) = 1.0D-6
   20 CONTINUE
      IFAIL = 0
*
      CALL D02QWF(STATEF,NEQF,VECTOL,ATOL,LATOL,RTOL,LRTOL,ONESTP,CRIT,
     +            TCRIT,HMAX,MAXSTP,NEQG,ALTERG,SOPHST,RWORK,LRWORK,
     +            IWORK,LIWORK,IFAIL)
*
      T = TSTART
      TOUT = TCRIT
      Y(1) = 0.0D0
      Y(2) = 1.0D0
*
   40 IFAIL = -1
*
      CALL D02QFF(FTRY02,NEQF,T,Y,TOUT,GTRY02,NEQG,ROOT,RWORK,LRWORK,
     +            IWORK,LIWORK,IFAIL)
*
      IF (IFAIL.EQ.0) THEN
*
         CALL D02QXF(NEQF,YP,TCURR,HLAST,HNEXT,ODLAST,ODNEXT,NSUCC,
     +               NFAIL,TOLFAC,BADCMP,RWORK,LRWORK,IWORK,LIWORK,
     +               IFAIL)
*
         IF (ROOT) THEN
*
            CALL D02QYF(NEQG,INDEX,TYPE,EVENTS,RESIDS,RWORK,LRWORK,
     +                  IWORK,LIWORK,IFAIL)
*
            WRITE (NOUT,*)
            WRITE (NOUT,99999) 'Root at ', T
            WRITE (NOUT,99998) 'for event equation ', INDEX,
     +        ' with type', TYPE, ' and residual ', RESIDS(INDEX)
            WRITE (NOUT,99999) ' Y(1) = ', Y(1), '   Y''(1) = ', YP(1)
            DO 60 I = 1, NEQG
               IF (I.NE.INDEX) THEN
                  IF (EVENTS(I).NE.0) THEN
                     WRITE (NOUT,99998) 'and also for event equation ',
     +                 I, ' with type', EVENTS(I), ' and residual ',
     +                 RESIDS(I)
                  END IF
               END IF
   60       CONTINUE
            IF (TCURR.LT.TOUT) GO TO 40
         END IF
      END IF
      STOP
*
99999 FORMAT (1X,A,1P,D13.5,A,1P,D13.5)
99998 FORMAT (1X,A,I2,A,I3,A,1P,D13.5)
      END
*
      SUBROUTINE FTRY02(NEQF,T,Y,YP)
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
*
      DOUBLE PRECISION FUNCTION GTRY02(NEQF,T,Y,YP,K)
*     .. Scalar Arguments ..
      DOUBLE PRECISION                 T
      INTEGER                          K, NEQF
*     .. Array Arguments ..
      DOUBLE PRECISION                 Y(NEQF), YP(NEQF)
*     .. Executable Statements ..
      IF (K.EQ.1) THEN
         GTRY02 = YP(1)
      ELSE
         GTRY02 = Y(1)
      END IF
      RETURN
      END
