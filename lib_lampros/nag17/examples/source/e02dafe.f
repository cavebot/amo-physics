*     E02DAF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          MMAX, MAXPX, MAXPY, NCMAX, IP, NIWS, NWS, NADRES,
     +                 NPTMAX
      PARAMETER        (MMAX=40,MAXPX=10,MAXPY=10,NCMAX=(MAXPX-4)
     +                 *(MAXPY-4),IP=3*(MAXPY-4)+4,NIWS=MAXPY-4,
     +                 NWS=2*NCMAX*(IP+2)+IP,NADRES=(MAXPX-7)*(MAXPY-7),
     +                 NPTMAX=MMAX+NADRES)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION EPS, SIGMA, SUM, TEMP
      INTEGER          I, IADRES, IFAIL, ITEMP, J, M, NC, NP, PX, PY,
     +                 RANK
*     .. Local Arrays ..
      DOUBLE PRECISION C(NCMAX), DL(NCMAX), F(MMAX), FF(MMAX),
     +                 LAMDA(MAXPX), MU(MAXPY), W(MMAX), WS(NWS),
     +                 X(MMAX), Y(MMAX)
      INTEGER          ADRES(NADRES), IWS(NIWS), POINT(NPTMAX)
      CHARACTER*1      LABEL(2)
*     .. External Subroutines ..
      EXTERNAL         E02DAF, E02DEF, E02ZAF
*     .. Data statements ..
      DATA             LABEL/'X', 'Y'/
*     .. Executable Statements ..
      WRITE (NOUT,*) 'E02DAF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
   20 READ (NIN,*,END=140) EPS
*     Read data, interchanging X and Y axes if PX.LT.PY
      READ (NIN,*) M
      IF (M.LE.MMAX .AND. M.GT.0) THEN
         READ (NIN,*) PX, PY
         IF (PX.GE.8 .AND. PX.LE.MAXPX .AND. PY.GE.8 .AND. PY.LE.MAXPY)
     +       THEN
            IF (PX.LT.PY) THEN
               ITEMP = PX
               PX = PY
               PY = ITEMP
               ITEMP = 1
               READ (NIN,*) (Y(I),X(I),F(I),W(I),I=1,M)
               IF (PY.GT.8) READ (NIN,*) (MU(J),J=5,PY-4)
               IF (PX.GT.8) READ (NIN,*) (LAMDA(J),J=5,PX-4)
            ELSE
               ITEMP = 0
               READ (NIN,*) (X(I),Y(I),F(I),W(I),I=1,M)
               IF (PX.GT.8) READ (NIN,*) (LAMDA(J),J=5,PX-4)
               IF (PY.GT.8) READ (NIN,*) (MU(J),J=5,PY-4)
            END IF
            NC = (PX-4)*(PY-4)
            NP = (PX-7)*(PY-7)
            WRITE (NOUT,*)
            WRITE (NOUT,99995) 'Interior ', LABEL(ITEMP+1), '-knots'
            DO 40 J = 5, PX - 4
               WRITE (NOUT,99996) LAMDA(J)
   40       CONTINUE
            IF (PX.EQ.8) WRITE (NOUT,*) 'None'
            WRITE (NOUT,*)
            WRITE (NOUT,99995) 'Interior ', LABEL(2-ITEMP), '-knots'
            DO 60 J = 5, PY - 4
               WRITE (NOUT,99996) MU(J)
   60       CONTINUE
            IF (PY.EQ.8) WRITE (NOUT,*) 'None'
*           Sort points into panel order
            IFAIL = 0
*
            CALL E02ZAF(PX,PY,LAMDA,MU,M,X,Y,POINT,NPTMAX,ADRES,NP,
     +                  IFAIL)
*
*           Fit bicubic spline to data points
            IFAIL = 0
*
            CALL E02DAF(M,PX,PY,X,Y,F,W,LAMDA,MU,POINT,NPTMAX,DL,C,NC,
     +                  WS,NWS,EPS,SIGMA,RANK,IFAIL)
*
            WRITE (NOUT,*)
            WRITE (NOUT,99999) 'Sum of squares of residual RHS', SIGMA
            WRITE (NOUT,*)
            WRITE (NOUT,99998) 'Rank', RANK
*           Evaluate spline at the data points
            IFAIL = 0
*
            CALL E02DEF(M,PX,PY,X,Y,LAMDA,MU,C,FF,WS,IWS,IFAIL)
*
            SUM = 0
            IF (ITEMP.EQ.1) THEN
               WRITE (NOUT,*)
               WRITE (NOUT,*) 'X and Y have been interchanged'
            END IF
*           Output data points, fitted values and residuals
            WRITE (NOUT,*)
            WRITE (NOUT,*)
     +        '      X          Y          Data       Fit     Residual'
            DO 100 I = 1, NP
               IADRES = I + M
   80          IADRES = POINT(IADRES)
               IF (IADRES.LE.0) GO TO 100
               TEMP = FF(IADRES) - F(IADRES)
               WRITE (NOUT,99997) X(IADRES), Y(IADRES), F(IADRES),
     +           FF(IADRES), TEMP
               SUM = SUM + (TEMP*W(IADRES))**2
               GO TO 80
  100       CONTINUE
            WRITE (NOUT,*)
            WRITE (NOUT,99999) 'Sum of squared residuals', SUM
            WRITE (NOUT,*)
            WRITE (NOUT,*) 'Spline coefficients'
            DO 120 I = 1, PX - 4
               WRITE (NOUT,99996) (C((I-1)*(PY-4)+J),J=1,PY-4)
  120       CONTINUE
            GO TO 20
         END IF
      END IF
  140 STOP
*
99999 FORMAT (1X,A,1P,D16.2)
99998 FORMAT (1X,A,I5)
99997 FORMAT (1X,4F11.4,D11.2)
99996 FORMAT (1X,6F11.4)
99995 FORMAT (1X,A,A1,A)
      END
