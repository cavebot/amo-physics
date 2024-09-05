*     E01DAF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          MXMAX, MYMAX
      PARAMETER        (MXMAX=20,MYMAX=MXMAX)
      INTEGER          LIWRK, LWRK
      PARAMETER        (LIWRK=MXMAX+2*(MXMAX-3)*(MYMAX-3),LWRK=(MXMAX+6)
     +                 *(MYMAX+6))
*     .. Local Scalars ..
      DOUBLE PRECISION STEP, XHI, XLO, YHI, YLO
      INTEGER          I, IFAIL, J, MX, MY, NX, NY, PX, PY
*     .. Local Arrays ..
      DOUBLE PRECISION C(MXMAX*MYMAX), F(MXMAX*MYMAX), FG(MXMAX*MYMAX),
     +                 LAMDA(MXMAX+4), MU(MYMAX+4), TX(MXMAX),
     +                 TY(MYMAX), WRK(LWRK), X(MXMAX), Y(MYMAX)
      INTEGER          IWRK(LIWRK)
      CHARACTER*10     CLABS(MYMAX), RLABS(MXMAX)
*     .. External Subroutines ..
      EXTERNAL         E01DAF, E02DFF, X04CBF
*     .. Intrinsic Functions ..
      INTRINSIC        MAX, MIN
*     .. Executable Statements ..
      WRITE (NOUT,*) 'E01DAF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
*     Read the number of X points, MX, and the values of the
*     X co-ordinates.
      READ (NIN,*) MX
      READ (NIN,*) (X(I),I=1,MX)
*     Read the number of Y points, MY, and the values of the
*     Y co-ordinates.
      READ (NIN,*) MY
      READ (NIN,*) (Y(I),I=1,MY)
*     Read the function values at the grid points.
      DO 20 J = 1, MY
         READ (NIN,*) (F(MY*(I-1)+J),I=1,MX)
   20 CONTINUE
      IFAIL = 0
*
*     Generate the (X,Y,F) interpolating bicubic B-spline.
      CALL E01DAF(MX,MY,X,Y,F,PX,PY,LAMDA,MU,C,WRK,IFAIL)
*
*     Print the knot sets, LAMDA and MU.
      WRITE (NOUT,*)
      WRITE (NOUT,*)
     +  '               I   Knot LAMDA(I)      J     Knot MU(J)'
      DO 40 J = 4, MAX(PX,PY) - 3
         IF (J.LE.PX-3 .AND. J.LE.PY-3) THEN
            WRITE (NOUT,99997) J, LAMDA(J), J, MU(J)
         ELSE IF (J.LE.PX-3) THEN
            WRITE (NOUT,99997) J, LAMDA(J)
         ELSE IF (J.LE.PY-3) THEN
            WRITE (NOUT,99996) J, MU(J)
         END IF
   40 CONTINUE
*     Print the spline coefficients.
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'The B-Spline coefficients:'
      WRITE (NOUT,99999) (C(I),I=1,MX*MY)
      WRITE (NOUT,*)
*     Evaluate the spline on a regular rectangular grid at NX*NY
*     points over the domain (XLO to XHI) x (YLO to YHI).
      READ (NIN,*) NX, XLO, XHI
      READ (NIN,*) NY, YLO, YHI
      IF (NX.LE.MXMAX .AND. NY.LE.MYMAX) THEN
         STEP = (XHI-XLO)/(NX-1)
         DO 60 I = 1, NX
*           Generate NX equispaced X co-ordinates.
            TX(I) = MIN(XLO+(I-1)*STEP,XHI)
*           Generate X axis labels for printing results.
            WRITE (CLABS(I),99998) TX(I)
   60    CONTINUE
         STEP = (YHI-YLO)/(NY-1)
         DO 80 I = 1, NY
            TY(I) = MIN(YLO+(I-1)*STEP,YHI)
            WRITE (RLABS(I),99998) TY(I)
   80    CONTINUE
*
*        Evaluate the spline.
         CALL E02DFF(NX,NY,PX,PY,TX,TY,LAMDA,MU,C,FG,WRK,LWRK,IWRK,
     +               LIWRK,IFAIL)
*
*        Print the results.
         CALL X04CBF('General','X',NY,NX,FG,NY,'F8.3',
     +          'Spline evaluated on a regular mesh (X across, Y down):'
     +               ,'Character',RLABS,'Character',CLABS,80,0,IFAIL)
*
      END IF
      STOP
*
99999 FORMAT (1X,8F9.4)
99998 FORMAT (F5.2)
99997 FORMAT (1X,I16,F12.4,I11,F12.4)
99996 FORMAT (1X,I39,F12.4)
      END
