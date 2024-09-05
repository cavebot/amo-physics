      SUBROUTINE G01AHF(X,NOBS,NSTEPX,NSTEPY,ISTAND,IWORK,WORK,LWORK,
     *                  XSORT,XBAR,XSTD,IFAIL)
C     MARK 14 RE-ISSUE. NAG COPYRIGHT 1989.
C     G01AHF PLOTS AN ARRAY OF RESIDUALS AGAINST THE NORMAL SCORES
C     FOR A SAMPLE OF THE SAME SIZE, ON A CHARACTER PRINTING DEVICE
C     WITH A CHOSEN NUMBER OF CHARACTER POSITIONS IN EACH DIRECTION
C     .. Parameters ..
      CHARACTER*6       SRNAME
      PARAMETER         (SRNAME='G01AHF')
C     .. Scalar Arguments ..
      DOUBLE PRECISION  XBAR, XSTD
      INTEGER           IFAIL, ISTAND, LWORK, NOBS, NSTEPX, NSTEPY
C     .. Array Arguments ..
      DOUBLE PRECISION  WORK(LWORK), X(NOBS), XSORT(NOBS)
      INTEGER           IWORK(NOBS)
C     .. Local Scalars ..
      DOUBLE PRECISION  AIX, AIY, ERR, ERREST, STEPX, STEPY, XMAX, XMIN,
     *                  XNMIN, YNMIN, ZMPN, ZMPP
      INTEGER           I, IFA, IFA2, IW, MAXAX, MAXAY, MAXBX, MAXBY,
     *                  NSX, NSY
C     .. Local Arrays ..
      CHARACTER*1       P01REC(1)
C     .. External Functions ..
      DOUBLE PRECISION  X02AJF
      INTEGER           P01ABF
      EXTERNAL          X02AJF, P01ABF
C     .. External Subroutines ..
      EXTERNAL          G01AGY, G01AGZ, G01AHZ, G01DAF, M01CAF
C     .. Intrinsic Functions ..
      INTRINSIC         AINT, MAX, MIN, DBLE, SQRT
C     .. Executable Statements ..
      IFA = 1
      IF (NOBS.LT.2) GO TO 160
      IFA = 3
      IF ((5*NOBS)/2.GT.LWORK) GO TO 160
      CALL G01AHZ(X,NOBS,XBAR,XSTD)
      IFA = 2
      IF (XSTD.LE.0.0D0) GO TO 160
      XSTD = SQRT(XSTD/DBLE(NOBS-1))
      IF (ISTAND.GT.0) GO TO 40
      DO 20 I = 1, NOBS
         XSORT(I) = X(I)
   20 CONTINUE
      GO TO 80
   40 DO 60 I = 1, NOBS
         XSORT(I) = (X(I)-XBAR)/XSTD
   60 CONTINUE
   80 IFA2 = 1
      CALL M01CAF(XSORT,1,NOBS,'D',IFA2)
      XMIN = XSORT(NOBS)
      XMAX = XSORT(1)
      NSX = MAX(MIN(NSTEPX,133),10)
      CALL G01AGZ(XMIN,XMAX,NSX,XNMIN,STEPX,MAXAX,MAXBX)
      ZMPP = 1.0D0 + 2.0D0*X02AJF()
      ZMPN = 1.0D0 - 2.0D0*X02AJF()
      AIX = AINT(ZMPP*(XNMIN/STEPX))
      IF (XNMIN.LT.0.0D0 .AND. (AIX*STEPX).GT.(ZMPN*XNMIN)) AIX = AIX -
     *    1.D0
      IF (XMIN.GT.(AIX+0.5D0)*STEPX) GO TO 100
      XNMIN = XNMIN - STEPX
      NSX = NSX + 1
  100 IFA2 = 1
      ERR = 10.0D0**(-DBLE(MIN(4,MAXBX)))
      IW = (NOBS/2)*3
      CALL G01DAF(NOBS,WORK,ERR,ERREST,WORK(NOBS+1),IW,IFA2)
      DO 120 I = 1, NOBS
         WORK(I) = -WORK(I)
         IWORK(I) = I
  120 CONTINUE
      NSY = MAX(NSTEPY,10)
      CALL G01AGZ(WORK(NOBS),WORK(1),NSY,YNMIN,STEPY,MAXAY,MAXBY)
      AIY = AINT(ZMPP*(YNMIN/STEPY))
      IF (YNMIN.LT.0.0D0 .AND. AIY*STEPY.GT.ZMPN*YNMIN) AIY = AIY -
     *    1.0D0
      IF (WORK(NOBS).GT.(AIY+0.5D0)*STEPY) GO TO 140
      YNMIN = YNMIN - STEPY
      NSY = NSY + 1
  140 CALL G01AGY(XSORT,WORK,XNMIN,STEPX,NSX,YNMIN,STEPY,NSY,NOBS,MAXAX,
     *            MAXBX,IWORK,MAXAY,MAXBY)
      IFAIL = 0
      GO TO 180
  160 IFAIL = P01ABF(IFAIL,IFA,SRNAME,0,P01REC)
  180 RETURN
      END
