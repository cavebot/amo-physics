*     E02DCF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          MXMAX, MYMAX, NXEST, NYEST, NMAX, LWRK, LIWRK
      PARAMETER        (MXMAX=11,MYMAX=9,NXEST=MXMAX+4,NYEST=MYMAX+4,
     +                 NMAX=7,LWRK=4*(MXMAX+MYMAX)+11*(NXEST+NYEST)
     +                 +NXEST*MYMAX+NXEST+54,LIWRK=3+MXMAX+MYMAX+NXEST+
     +                 NYEST)
*     .. Local Scalars ..
      DOUBLE PRECISION DELTA, FP, S, XHI, XLO, YHI, YLO
      INTEGER          I, IFAIL, J, MX, MY, NPX, NPY, NX, NY
      CHARACTER        START
*     .. Local Arrays ..
      DOUBLE PRECISION C((NXEST-4)*(NYEST-4)), F(MXMAX*MYMAX),
     +                 FG(NMAX*NMAX), LAMDA(NXEST), MU(NYEST), PX(NMAX),
     +                 PY(NMAX), WRK(LWRK), X(MXMAX), Y(MYMAX)
      INTEGER          IWRK(LIWRK)
*     .. External Subroutines ..
      EXTERNAL         CPRINT, E02DCF, E02DFF
*     .. Intrinsic Functions ..
      INTRINSIC        MAX, MIN
*     .. Executable Statements ..
      WRITE (NOUT,*) 'E02DCF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
*     Input the number of X, Y co-ordinates MX, MY.
      READ (NIN,*) MX, MY
      IF (MX.GT.0 .AND. MX.LE.MXMAX .AND. MY.GT.0 .AND. MY.LE.MYMAX)
     +    THEN
*        Input the X co-ordinates followed by the Y co-ordinates.
         READ (NIN,*) (X(I),I=1,MX)
         READ (NIN,*) (Y(I),I=1,MY)
*        Input the MX*MY function values F at the grid points.
         READ (NIN,*) (F(I),I=1,MX*MY)
         START = 'Cold Start'
         READ (NIN,*,END=100) S
*        Determine the spline approximation.
         IFAIL = 0
*
         CALL E02DCF(START,MX,X,MY,Y,F,S,NXEST,NYEST,NX,LAMDA,NY,MU,C,
     +               FP,WRK,LWRK,IWRK,LIWRK,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,99999) 'Calling with smoothing factor S =', S,
     +     ': NX =', NX, ', NY =', NY, '.'
         WRITE (NOUT,*)
         WRITE (NOUT,*)
     +     '               I   Knot LAMDA(I)      J     Knot MU(J)'
         WRITE (NOUT,*)
         DO 20 J = 4, MAX(NX,NY) - 3
            IF (J.LE.NX-3 .AND. J.LE.NY-3) THEN
               WRITE (NOUT,99997) J, LAMDA(J), J, MU(J)
            ELSE IF (J.LE.NX-3) THEN
               WRITE (NOUT,99997) J, LAMDA(J)
            ELSE IF (J.LE.NY-3) THEN
               WRITE (NOUT,99996) J, MU(J)
            END IF
   20    CONTINUE
         CALL CPRINT(C,NY,NX,NOUT)
         WRITE (NOUT,*)
         WRITE (NOUT,99998) 'Sum of squared residuals FP =', FP
         IF (FP.EQ.0.0D+0) THEN
            WRITE (NOUT,*) '(The spline is an interpolating spline)'
         ELSE IF (NX.EQ.8 .AND. NY.EQ.8) THEN
            WRITE (NOUT,*)
     +        '(The spline is the least-squares bi-cubic polynomial)'
         END IF
*        Evaluate the spline on a rectangular grid at NPX*NPY points
*        over the domain (XLO to XHI) x (YLO to YHI).
         READ (NIN,*) NPX, XLO, XHI
         READ (NIN,*) NPY, YLO, YHI
         IF (NPX.LE.NMAX .AND. NPY.LE.NMAX) THEN
            DELTA = (XHI-XLO)/(NPX-1)
            DO 40 I = 1, NPX
               PX(I) = MIN(XLO+(I-1)*DELTA,XHI)
   40       CONTINUE
            DO 60 I = 1, NPY
               PY(I) = MIN(YLO+(I-1)*DELTA,YHI)
   60       CONTINUE
*
            CALL E02DFF(NPX,NPY,NX,NY,PX,PY,LAMDA,MU,C,FG,WRK,LWRK,IWRK,
     +                  LIWRK,IFAIL)
*
            WRITE (NOUT,*)
            WRITE (NOUT,*) 'Values of computed spline:'
            WRITE (NOUT,*)
            WRITE (NOUT,99995) '          X', (PX(I),I=1,NPX)
            WRITE (NOUT,*) '     Y'
            DO 80 I = NPY, 1, -1
               WRITE (NOUT,99994) PY(I), (FG(NPY*(J-1)+I),J=1,NPX)
   80       CONTINUE
         END IF
      END IF
  100 CONTINUE
*
99999 FORMAT (1X,A,1P,D13.4,A,I5,A,I5,A)
99998 FORMAT (1X,A,1P,D13.4)
99997 FORMAT (1X,I16,F12.4,I11,F12.4)
99996 FORMAT (1X,I39,F12.4)
99995 FORMAT (1X,A,7F8.2)
99994 FORMAT (1X,F8.2,3X,7F8.2)
      END
*
      SUBROUTINE CPRINT(C,NY,NX,NOUT)
*     .. Scalar Arguments ..
      INTEGER           NOUT, NX, NY
*     .. Array Arguments ..
      DOUBLE PRECISION  C(NY-4,NX-4)
*     .. Local Scalars ..
      INTEGER           I, J
*     .. Executable Statements ..
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'The B-spline coefficients:'
      WRITE (NOUT,*)
      DO 20 I = 1, NY - 4
         WRITE (NOUT,99999) (C(I,J),J=1,NX-4)
   20 CONTINUE
      RETURN
*
99999 FORMAT (1X,8F9.4)
      END
